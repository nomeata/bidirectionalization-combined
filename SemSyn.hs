{-# OPTIONS -XFlexibleInstances #-}

module SemSyn where

import Text.PrettyPrint
import Control.Monad.State
import Control.Monad.Error
import Data.List
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace 

import Data.Function (on)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph 

import Util 
import AST

import Parser
import Type
import Shapify
import CodeGen 


data Config 
    = Config 
      { 
        inputFile   :: Maybe String, -- ^ Path to input file
        execMode    :: ExecMode,  
        b18nMode    :: B18nMode, 
        outputMode  :: OutputMode,   -- ^ Obsolete   
        isHaskellify :: Bool, 
        isShowType  :: Bool
      }
    deriving Show 

data ExecMode 
    = Normal | Shapify | ShapifyPlus | Help | Debug 
    deriving (Eq, Read, Show)

data OutputMode = PseudoCode | HaskellCode | ForwardCode | OM_NotSpecified 
    deriving (Eq, Read, Show)

data B18nMode = SyntacticB18n | SemanticB18n | CombinedB18n | NoB18n 
    deriving (Eq, Read, Show)

defaultConfig = Config { 
                  inputFile    = Nothing, 
                  execMode     = Normal, 
                  b18nMode     = CombinedB18n, 
                  outputMode   = OM_NotSpecified, 
                  isHaskellify = False, 
                  isShowType   = True  }

-- | Since some combination of the config options is useless, 
--   this function adjust some configuration to valid one.
adjustConfig :: Config -> Config 
-- adjustConfig (conf@(Config {b18nMode = CombinedB18n})) =
--     conf { isHaskellify = True, execMode = ShapifyPlus }
-- adjustConfig (conf@(Config {b18nMode = SemanticB18n})) =
--     conf { isHaskellify = True, execMode = Normal, outputMode = ForwardCode }
-- adjustConfig (conf@(Config {b18nMode = SyntacticB18n})) =
--     conf { execMode = Normal, outputMode = PseudoCode }
adjustConfig (conf@(Config {outputMode = ForwardCode})) = 
    conf { b18nMode = NoB18n }
adjustConfig (conf@(Config {outputMode = HaskellCode})) | execMode conf /= Help = 
    conf { execMode = ShapifyPlus, isHaskellify = True, b18nMode = CombinedB18n  }
adjustConfig (conf@(Config {outputMode = PseudoCode})) =
    conf { isHaskellify = False, b18nMode = SyntacticB18n }
adjustConfig conf = conf 



outputCode :: Config -> Bool -> AST -> AST -> Doc
outputCode conf_ isShapify orig ast = 
    let (p1,p2,p3) = constructBwdFunction ast
    in case b18nMode conf of 
         NoB18n -> 
             if isHaskellify conf then 
                 ppr (generateCodeDet ast) 
             else
                 ppr (typeFilter ast)
         SyntacticB18n ->
             if isHaskellify conf then 
                 vcat [ text "import Control.Monad" 
                      , text "import BUtil"
                      , ppr (constructTypeDecl p2)
                      , ppr $ generateCodeBwd (orig, p1, p2, p3) ]
             else 
                 vcat [ ppr (constructTypeDecl p2)
                      , ppr orig $$ ppr (typeFilter p1) $$ ppr (typeFilter p2) $$ ppr (typeFilterT p3) ]
         SemanticB18n -> vcat $ 
             [ text "import Data.Bff" ] ++
             [ text "import BUtil" ] ++ 
             (map genBwdDefBff $ 
                   let AST decls = typeInference orig 
                   in map (\(Decl f t _ _:_) -> f) $ groupBy isSameFunc decls) ++
             [ ppr $ generateCodeDet p1 ]             
         CombinedB18n -> vcat $ 
             [ text "import Control.Monad"
             , text "import BUtil"
             ] ++ (
             if isShapify
             then map genBwdDef $
                     let AST decls = typeInference orig
                     in map (\(Decl f t _ _:_) -> (f,t)) $ groupBy isSameFunc decls
             else []                                     
             ) ++
             [ ppr (constructTypeDecl p2)
             , ppr $ generateCodeBwd (orig,p1,p2,p3)
             ]
                             
-- case outputMode conf of 
--          ForwardCode ->
--                   ppr (typeFilter ast)
--          PseudoCode  -> vcat
--                 [ ppr (constructTypeDecl p2)
--                 , ppr orig $$ ppr (typeFilter p1) $$ ppr (typeFilter p2) $$ ppr (typeFilterT p3)
--                 ]
--          HaskellCode -> vcat $
--                 [ text "import Control.Monad"
--                 , text "import BUtil"
--                 ] ++ (
--                 if isShapify
--                 then map genBwdDef $
--                         let AST decls = typeInference orig
--                         in map (\(Decl f t _ _:_) -> (f,t)) $ groupBy isSameFunc decls
--                 else []                                     
--                 ) ++
--                 [ ppr (constructTypeDecl p2)
--                 , ppr $ generateCodeBwd (orig,p1,p2,p3)
--                 ]
    where
      conf       = adjustConfig conf_
      typeFilter  = if isShowType conf then id else eraseType
      typeFilterT = if isShowType conf then id else eraseTypeT
      genBwdDefBff (Name fName) =
          ppr (Name fName) <> text "_B" $$
              nest 4 (text "= bff " <> ppr (Name fName)) $$
          ppr (Name fName) <> text "_B_Eq" $$
              nest 4 (text "= bff_Eq " <> ppr (Name fName)) $$
          ppr (Name fName) <> text "_B_Ord" $$
              nest 4 (text "= bff_Ord " <> ppr (Name fName))  
      genBwdDef (Name fName,(TFun is ts t)) =
          case (ts,t) of 
            ([TCon (Name "List") [TVar i]],TCon (Name "List") [TVar j]) | i == j  ->
                ppr (Name fName) <> text "_Bb bias s v" $$
                  nest 4 (text "= gen_put_bias bias Main." <> ppr (Name fName) 
                               <> text "(\\x y -> castError $ (" 
                               <> ppr (NBwd $ IName fName is)
                               <> text " $! x) $! y) s v" ) $$
                ppr (Name fName) <> text "_Bbd = withDefaultBias "
                    <> ppr (Name fName) <> text "_Bb" $$
                ppr (Name fName) <> text "_Bd = withDefault "
                    <> ppr (Name fName) <> text "_B" $$
                ppr (Name fName) <> text "_B s v = " 
                  <> ppr (Name fName) <> text "_Bb rear s v" 
            _ ->
                empty
                                  


checkTreeless :: AST -> Bool
checkTreeless (AST decls) = all checkTreelessD decls
    where
      checkTreelessD (Decl _ _ _ e) = checkTreelessE e
      checkTreelessE (EVar _ _ _)    = True
      checkTreelessE (ECon _ _ _ es) = all checkTreelessE es
      checkTreelessE (EFun _ _ _ es) = all isVariable es
      isVariable (EVar _ _ _) = True
      isVariable _            = False

checkAffine :: AST -> Bool 
checkAffine (AST decls) = all checkAffineD decls
    where
      checkAffineD (Decl _ _ _ e) = checkAffineE e 
      checkAffineE e = (varsE e == snub (varsE e))
      varsE (EVar _ _ v) = [v]
      varsE (ECon _ _ _ es) = concatMap varsE es
      varsE (EFun _ _ _ es) = concatMap varsE es


           
type Automaton s = Map s [Deriv s]
data Deriv s    = DCon Name [s]
                | DEps s 
                | DAny

pprAM :: (Ppr a, Ppr t) => Map t [Deriv a] -> Doc
pprAM am = 
    vcat $ map pprDs $ Map.toList am
    where
      pprDs (i,ds) = 
          ppr i $$
          nest 4 (vcat $ map (\d -> text " -> " <> pprD d) ds)
      pprD (DAny)      = text "_" 
      pprD (DEps s)    = ppr s
      pprD (DCon c ss) = ppr c <> parens (hcat $ punctuate comma (map ppr ss))

data AState = ASType Int
            | ASExp  Int
            | ASFun  Name
     deriving (Eq,Ord)

instance Ppr AState where
    ppr (ASType i) = text $ "T" ++ show i 
    ppr (ASExp  x) = text $ "E" ++ show x
    ppr (ASFun  n) = text $ "F" ++ show n

instance Show AState where
    show = show . ppr 

initTAMap :: Type -> State Int (AState, Map AState [Deriv AState])
initTAMap s = constructTypeMap s
    where
      uniq = do { i <- get; put (i+1); return i }
      listType t =
          do { (st,m) <- constructTypeMap t
             ; i <- uniq
             ; let m1 = Map.insert (ASType i) [DCon (Name "Nil")[], 
                                               DCon (Name "Cons") [ASType i, st]] m
             ; return (ASType i, m1) }
      natType = 
          do { let m1 = Map.insert (ASType 0) [DCon (Name "Z") [], 
                                               DCon (Name "S") [ASType 0]] (Map.empty)
             ; return (ASType 0, m1) }
      anyType =
          do { let m1 = Map.insert (ASType 1) [DAny] (Map.empty)
             ; return (ASType 1, m1) }
      constructTypeMap (TCon (Name "List") [t]) =
          listType t
      constructTypeMap (TCon (Name "Nat") []) =
          natType 
      constructTypeMap (TVar i) = 
          anyType
      constructTypeMap _ =
          anyType -- Not Supported

testOverlap :: AST -> Doc 
testOverlap (AST decl) = 
    let fDs = groupBy isSameFunc decl
    in evalState (do { docs <- mapM f fDs
                     ; return $ vcat docs }) initCAConf 
    where
      am = constructAutomaton (AST decl) initTAMap 
      f (ds@(Decl g t ps e:es)) = 
          do { b <- checkInjectivity am (AST decl) g
             ; let di  = text "Injectivity: " <> text (show $ b)
             ; ba <- checkAmbiguity am (ASFun g)
             ; let da  = text "Ambiguity: " <> text (show $ ba)
             ; dol <- pol ds 
             ; return $ text (show g) $$ nest 4 (da $$ di $$ dol) }
      pol ds = let is  = [ ASExp $ fromJust $ idofE e | (Decl f t ps e) <- ds ] 
                   ijs = [ (i,j) | vs <- [zip is [0..]], (i,x) <- vs, (j,y) <- vs ]
               in 
                 do { bs <- mapM (uncurry $ checkOverlapS am (Set.empty)) ijs 
                    ; let vs = map (map snd) $ groupBy ((==) `on` (fst.fst)) $ zip ijs bs 
                    ; return $ vcat ( map g vs )}
          where g bs = hcat ( map (\h -> if h then text "O" else text "-") bs)
          
isNonErasing :: Decl -> Bool
isNonErasing (Decl _ _ ps e) =
    null $ (snub $ concatMap varsP ps) \\ varsE e


checkInjectivity :: Automaton AState -> AST -> Name -> State (CAConf AState) Bool
checkInjectivity am (AST decl) f =
    checkInj (Set.empty) f 
    where
      checkInj env f =
          do { inj  <- getInjSet
             ; ninj <- getNonInjSet
             ; if Set.member f inj || Set.member f env then 
                   return True
               else if Set.member f ninj then 
                   return False
               else
                   do { b1 <- checkAmbiguity am (ASFun f)
                      ; when b1 (addNonInj f)
                      ; let dsF = filter (\(Decl g _ _ _) -> g == f) decl
                      ; let b2  = all isNonErasing dsF
                      ; when (not b2) (addNonInj f )
                      ; let fs = concatMap calledFunctions dsF
                      ; b3  <- mapM (checkInj (Set.insert f env)) fs
                      ; when (not $ and b3) (addNonInj f)
                      ; if (not b1) && b2 && and b3 then
                            when (Set.null env) (addInj f) >> return True
                        else
                            return False } }

testTupling :: AST -> TAST
testTupling ast = 
    evalState (do { cAst <- optimizedComplementation am ast
                  ; tAst <- doTupling am ast cAst
                  ; return tAst }) initCAConf
    where
      am = constructAutomaton ast initTAMap 


data TypeDecl = TypeDecl Name [Int] [ConDef]
data ConDef   = ConDef Name [Type] -- NB: First Order

instance Ppr TypeDecl where
    ppr (TypeDecl c is cdefs) = 
        text "data" <+> ppr c <+>
             hsep (map (\i -> text "t" <> ppr i) is) $$ nest 4 (ppr cdefs)
    pprList decls =
        vcat $ map ppr decls

instance Ppr ConDef where
    ppr (ConDef n ts) = ppr n <+> hsep (map pprT ts)
        where
          pprT (TVar i) = ppr (TVar i)
          pprT t        = parens (ppr t)
    pprList []        = empty 
    pprList (c:cs)    = text "=" <+> ppr c $$
                        f cs
        where f []     = empty 
              f [c]    = text "|" <+> ppr c 
              f (c:cs) = text "|" <+> ppr c $$ f cs  

-- Extract Type Definition from the complement definition
-- FIXME: More Smart Definition
constructTypeDecl :: AST -> [ TypeDecl ]
constructTypeDecl (AST cdecls) = [ TypeDecl (cmplName) vs condef ]
    where
      cmplName = Name "Cmpl"
      vs = snub $ concat $ evalState (mapM extractVarsT cdecls) (0,[])
      extractVarsT (Decl _ _ _ e) =
          case e of 
            (ECon _ _ c es) -> 
                do { trav <- traversedList
                   ; if c `elem` trav then 
                         return []
                     else
                         do { addTraversed c 
                            ; let ws = [ w | (EVar _ _ w) <- es ]
                            ; is <- mapM (\_ -> uniq) ws 
                            ; return $ is }}
            _ -> 
                return []
      uniq = do { (i,t) <- get; put (i+1,t); return i }
      traversedList  = do { (i,t) <- get; return t }
      addTraversed t = do { (i,tt) <- get; put (i,t:tt) }
      condef = concat $ evalState (mapM mkConDef cdecls) (0,[])
      mkConDef (Decl _ _ _ e) =
          case e of 
            (ECon _ _ c es) -> 
                do { trav <- traversedList
                   ; if c `elem` trav then 
                         return []
                     else 
                         do { addTraversed c 
                            ; let ws = [ w | (EVar _ _ w) <- es ]
                            ; let fs = [ TCon cmplName [ TVar i | i <- vs ] | (EFun _ _ _ _) <- es ]
                            ; is <- mapM (\_ -> uniq) ws 
                            ; return $ [ ConDef c (map TVar is ++ fs) ] } }
            _ ->
                return [] 
                  




constructBwdFunction :: AST -> (AST, AST, TAST) 
constructBwdFunction ast =
    evalState ( do { cmpl <- optimizedComplementation am ast
                   ; tpl  <- doTupling am ast cmpl 
                   ; let itpl = localInversion tpl 
                   ; let fs = functions ast
                   ; bwds <- mapM constructBWD fs 
                   ; return $ (AST bwds, cmpl, itpl)
                   }) initCAConf
    where
      am = constructAutomaton ast initTAMap 
      functions (AST decls) = map (\(Decl f _ _ _:_) -> f) $ groupBy isSameFunc decls 
      pS = PVar Nothing TUndet (Name "s")
      pV = PVar Nothing TUndet (Name "v")
      eS = EVar Nothing TUndet (Name "s")
      eV = EVar Nothing TUndet (Name "v")
      constructBWD f = do { b <- checkInjectivity am ast f 
                          ; if b then 
                                return $ Decl (NBwd f) FTUndet [pS,pV] 
                                           (EFun Nothing TUndet (NInv f) [eV])
                            else
                                return $ Decl (NBwd f) FTUndet [pS,pV]
                                           (EFun Nothing TUndet (NInv (NTuple f)) 
                                                     [eV, EFun Nothing TUndet (NCmpl f) [eS]]) } 

localInversion :: TAST -> TAST
localInversion (TAST tdecls) = TAST $ map localInversionD tdecls

localInversionD :: TDecl -> TDecl 
localInversionD (TDecl f ps es vdecls) =
    TDecl (NInv f) (map e2p es) (map p2e ps) (map linvVD vdecls)
    where
      e2p (EVar i t v)    = PVar i t v 
      e2p (ECon i t c es) = PCon i t c (map e2p es)
      p2e (PVar i t v)    = EVar i t v 
      p2e (PCon i t c ps) = ECon i t c (map p2e ps)
      linvVD (VDecl vs f us) = VDecl us (NInv f) vs 
      
-- doTupling :: AST -> AST -> TAST 
doTupling :: Automaton AState -> AST -> AST -> State (CAConf AState) TAST
doTupling am (AST decls) (AST cdecls) =
    do { tdecls <- mapM tupleD $ zip decls cdecls 
       ; return $ TAST tdecls }
    where
      fCalls (EFun _ _ f es) = [(f,es)] 
      fCalls (ECon _ _ _ es) = concatMap fCalls es
      fCalls _               = []
      convE mp (EFun i t f es) = case lookup (f,es) mp of
                                   Just v  -> EVar i t v 
                                   Nothing -> EFun i t f es -- never happens
      convE mp (EVar i t v )   = EVar i t v 
      convE mp (ECon i t c es) = ECon i t c (map (convE mp) es)
      tupleD (Decl f _ ps e, Decl _ _ _ ce) =
          do { b <- checkInjectivity am (AST decls) f
             ; if b then 
                   return $ tupleDInj f ps e 
               else 
                   return $ tupleDNInj f ps e ce }
      tupleDInj f ps e = TDecl f ps [convE vnMap e] wdecls -- keep function-name if injective 
          where
            funCalls = fCalls e 
            vnMap    = zip funCalls [ NTupleV i | i <- [1..] ]
            wdecls = map (\((f,es),v) -> VDecl [v] f [u | (EVar _ _ u) <- es ]) vnMap 
      tupleDNInj f ps e ce = TDecl (NTuple f) ps [convE vnMap e, convE cnMap ce] wdecls 
          where
            funCalls  = fCalls e
            ninjCalls = [ (f,es) | (NCmpl f, es) <- fCalls ce ]
            injCalls  = funCalls \\ ninjCalls 
            vnMap    = zip funCalls [ NTupleV  i | i <- [1..] ]
            cnMap    = zip (map (\(f,es) -> (NCmpl f,es)) funCalls) [ NTupleVC i | i <- [1..] ]
            wdecls   = map convW vnMap 
                where 
                  convW ((f,es),v@(NTupleV i)) 
                      | (f,es) `elem` injCalls = VDecl [v] f [u | (EVar _ _ u) <- es ]
                      | otherwise              = VDecl [v,NTupleVC i] (NTuple f) [u | (EVar _ _ u) <- es ]
                                       
            
                                      
                          




                      
calledFunctions (Decl _ _ _ e) = snub $ calledFunctionsE e 
calledFunctionsE (ECon _ _ _ es) = concatMap calledFunctionsE es
calledFunctionsE (EFun _ _ f es) = f:concatMap calledFunctionsE es 
calledFunctionsE _               = []
                            

testComplementation (AST decls) = 
    evalState (optimizedComplementation am (AST decls)) initCAConf
    where
      am = constructAutomaton (AST decls) initTAMap 
      

optimizedComplementation am (AST decls) 
    = do { cmpl   <- complementation am (AST decls)
         ; cmpl'  <- optimizeByRemove am cmpl
         ; cmpl'' <- optimizeByRename am cmpl' 
         ; return $ cmpl'' }

optimizeByRename am (AST cdecls) =
    do { let cfdeclss = groupBy isSameFunc cdecls
       ; cfdeclss' <- mapM optRename cfdeclss 
       ; return $ AST $ concat cfdeclss' }
    where
      optRename cfdecls = 
         do { let ids = [ (i,[ (v,t) | (EVar _ t v) <- es  ],length es)  
                               | (Decl _ _ _ (ECon _ _ (NCConE i) es)) <- cfdecls ]
            ; idss <- grouping [] ids 
            ; return $ map (doRename idss) cfdecls }
--      grouping :: [[(Int,[(Name,Type)])]] -> [(Int,[(Name,Type)])] -> State (CAConf AState) [[(Int,[(Name,Type)])]]
      grouping gs []      = return gs 
      grouping gs (i:ids) =
         do { gs' <- checkGroup i gs 
            ; grouping gs' ids }
--      checkGroup :: (Int,[(Name,Type)]) -> [[(Int,[(Name,Type)])]] -> State (CAConf AState) [[(Int,[(Name,Type)])]]
      checkGroup i [] = return $ [ [i] ]
      checkGroup i (g:gs) =
          do { b <- compatible i g  
             ; if b then 
                   return $ (i:g):gs
               else
                   do { gs' <- checkGroup i gs 
                      ; return $ g:gs' } }
      compatible i [] = return $ True
      compatible (i,vs,l) ((i',vs',l'):ls) =
          do { b <- checkOverlapS am Set.empty (ASExp i) (ASExp i')
             ; let b2 = (sort $ map snd vs) == (sort $ map snd vs') 
             ; if (not b) && b2 && l == l' then 
                   compatible (i,vs,l) ls 
               else
                   return False }
      doRename idss (Decl f t ps (ECon ei et (NCConE i) es)) = 
          Decl f t ps (ECon ei et (NCConU f j) es') 
          where
            j = fix (\f n iss -> 
                         case iss of 
                           is:iss -> 
                               if i `elem` (map (\(a,_,_) -> a) is) then 
                                   n 
                               else 
                                   f (n+1) iss) 1 idss 
            es' = sortBy (compare `on` typeofE) [ e | (e@(EVar _ _ _)) <- es ]
                  ++ [ e | (e@(EFun _ _ _ _)) <- es ]
      doRename idss d = d 
            
          
          
                 

optimizeByRemove am (AST cdecls) =
    do { let cfdeclss = groupBy isSameFunc cdecls 
       ; cfdeclss' <- mapM optRem cfdeclss 
       ; return $ AST $ concat cfdeclss' }
    where
      fromInj (Decl _ _ _ (ECon _ _ (NCConF _) _)) = True
      fromInj _                                    = False
      optRem cfdecls 
          | fromInj (head cfdecls) = return cfdecls
          | otherwise = 
              mapM (\(Decl f t ps e) -> do { e' <- tryRemove e; return $ Decl f t ps e'}) cfdecls                 
          where ids = [ i | (Decl _ _ _ (ECon _ _ (NCConE i) _)) <- cfdecls ]
                tryRemove (ex@(ECon ei et (NCConE i) [EFun ei' et' f es])) = 
                    do { b <- checkNonOverlapAll i ids 
                       ; if b then 
                             return $ EFun ei' et' f es
                         else
                             return $ ex }
                tryRemove ex = return ex 
                checkNonOverlapAll i [] = return $ True
                checkNonOverlapAll i (j:jds) | i == j    = checkNonOverlapAll i jds 
                                             | otherwise = do { b <- checkOverlapS am (Set.empty) (ASExp i) (ASExp j)
                                                              ; if b then 
                                                                    return False
                                                                else
                                                                    checkNonOverlapAll i jds }

complementation am (ast@(AST decls)) =
    do { cdecls <- mapM (complementationD am ast) decls
       ; return $ AST cdecls }

complementationD am ast (Decl f ftype ps e) =                 
    do { b <- checkInjectivity am ast f
       ; if b then 
             return $ (Decl (NCmpl f) FTUndet ps (ECon Nothing TUndet cNameC []))
         else 
             do { let fs = fCalls e 
                ; fs' <- filterNonInj fs
                ; let e' = cExp unusedVars fs' 
                ; return $ (Decl (NCmpl f) FTUndet ps e') }
       }
    where
      unusedVars = snub $ (snub $ concatMap varsWithTP ps) \\ (snub $ varsWithTE e)
      varsWithTP (PVar _ t v)    = [EVar Nothing t v]
      varsWithTP (PCon _ _ _ ps) = concatMap varsWithTP ps
      varsWithTE (EVar _ t v)    = [EVar Nothing t v]
      varsWithTE (ECon _ _ _ es) = concatMap varsWithTE es 
      varsWithTE (EFun _ _ _ es) = concatMap varsWithTE es 
      fCalls (ECon _ _ _ es) = concatMap fCalls es 
      fCalls (EFun _ _ f es) = (f,es):concatMap fCalls es
      fCalls _               = []
      filterNonInj []          = return []
      filterNonInj ((f,es):fs) = do { b <- checkInjectivity am ast f 
                                    ; if b then 
                                          filterNonInj fs 
                                      else
                                          do { fs' <- filterNonInj fs
                                             ; return $ (f,es):fs' } }
      cNameC = NCConF f 
      cName  = NCConE (fromJust $ idofE e)
      cExp vs fs = ECon Nothing TUndet cName $ 
                       vs ++ (map (\(f,es) -> EFun Nothing TUndet (NCmpl f) es) fs)



constructAutomaton :: AST -> (Type -> State Int (AState,Automaton AState)) -> Automaton AState
constructAutomaton (AST decl) initTAMap =
    removeEps $ evalState (unionM $ map constructAMD decl) 2 
    where
      unionM []     = return $ Map.empty
      unionM (m:ms) = do { r1 <- unionM ms 
                         ; r2 <- m
                         ; return $ Map.unionWith (++) r1 r2 }
      constructAMD (Decl f t ps e) = 
          do { (st,m) <- constructAME e
             ; return $ Map.insertWith (++) (ASFun f) [DEps st] m }
      constructAME (ECon i t c es) =
          do { ims <- mapM constructAME es
             ; let (is,ms) = unzip ims
             ; let m       = Map.unions ms
             ; let s       = ASExp $ fromJust i 
             ; return (s, Map.insertWith (++) s [DCon c is] m)}
      constructAME (EFun i t f es) = -- Assume Treeless
          let s = ASExp $ fromJust i  
          in return (s, Map.insertWith (++) s [DEps $ ASFun f] (Map.empty))
      constructAME (EVar i t x) =
          do { (st,m) <- initTAMap t 
             ; let s = ASExp $ fromJust i 
             ; return (s, Map.insertWith (++) s [DEps st] m) }
     
allStates :: Ord s => Automaton s -> [s]
allStates am = 
    snub $ foldr (++) [] $ map (\(i,d) -> i:concatMap allStatesDerive d) $ Map.toList am 


allStatesDerive (DCon _ is) = is
allStatesDerive (DAny)      = []
allStatesDerive (DEps i)    = [i]

removeEps :: Automaton AState -> Automaton AState 
removeEps am = 
    foldr (Map.unionWith (++)) Map.empty $
         map f $ Map.toList am
    where
      f (i,ds) = Map.fromList [ (j,filter nonEpsD ds) | j <- reachableA i ]
      isEps (_,DEps _) = True
      isEps _          = False
      nonEpsD (DEps _) = False
      nonEpsD _        = True
      erules       = filter isEps $ concatMap (\(i,ds) -> [(i,d)|d<-ds]) $ Map.toList am
      aStates      = allStates am
      (graph,v2n,k2v) = graphFromEdges $ map (\(i,is) -> (i,i,is)) $
                            Map.toList $ Map.fromListWith (++) $
                              (map (\(i,DEps j) -> (j,[i])) erules
                               ++ map (\i->(i,[i])) aStates)
      reachableA k = map (\(n,_,_) -> n) $ map v2n $ reachable graph (fromJust $ k2v k) 
                           
              
type AmbSet   s = Set s
type UnAmbSet s = Set s
type OLSet    s = Set (s,s)
type UnOLSet  s = Set (s,s)
type InjSet     = Set Name
type NonInjSet  = Set Name
type CAConf   s = (AmbSet s, UnAmbSet s, OLSet s, UnOLSet s, InjSet, NonInjSet)

initCAConf = (Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty)

ambset    (s,_,_,_,_,_) = s
unambset  (_,s,_,_,_,_) = s 
olset     (_,_,s,_,_,_) = s 
unolset   (_,_,_,s,_,_) = s
injset    (_,_,_,_,s,_) = s  
noninjset (_,_,_,_,_,s) = s  

getAmbSet :: Ord s => State (CAConf s) (AmbSet s)
getAmbSet   = get >>= (return . ambset)

getUnAmbSet :: Ord s => State (CAConf s) (UnAmbSet s)
getUnAmbSet = get >>= (return . unambset)

getOLSet :: Ord s => State (CAConf s) (OLSet s)
getOLSet    = get >>= (return . olset)

getUnOLSet :: Ord s => State (CAConf s) (UnOLSet s)
getUnOLSet  = get >>= (return . unolset)

getInjSet :: State (CAConf s) InjSet
getInjSet   = get >>= (return . injset)

getNonInjSet :: State (CAConf s) NonInjSet
getNonInjSet = get >>= (return . noninjset)

addAmb    x = do { (a,ua,o,uo,inj,ninj) <- get; put (Set.insert x a,ua,o,uo,inj,ninj) }
addUnAmb  x = do { (a,ua,o,uo,inj,ninj) <- get; put (a,Set.insert x ua,o,uo,inj,ninj) }
addOL     x = do { (a,ua,o,uo,inj,ninj) <- get; put (a,ua,Set.insert x o,uo,inj,ninj) }
addUnOL   x = do { (a,ua,o,uo,inj,ninj) <- get; put (a,ua,o,Set.insert x uo,inj,ninj) }
addInj    x = do { (a,ua,o,uo,inj,ninj) <- get; put (a,ua,o,uo,Set.insert x inj,ninj) }
addNonInj x = do { (a,ua,o,uo,inj,ninj) <- get; put (a,ua,o,uo,inj,Set.insert x ninj) }

checkAmbiguity :: Ord s => Automaton s -> s -> State (CAConf s) Bool
checkAmbiguity am s = 
    do { amb    <- getAmbSet 
       ; unamb  <- getUnAmbSet
       ; if (Set.member s amb) then 
             return True
         else if (Set.member s unamb) then 
             return False
         else
             do { b <- checkAmb am unamb s 
                ; when (not b) (addUnAmb s)
                ; return b } }
    where
      distinctPairs ds = let vs = zip ds [0..]
                         in [ (d1,d2) | (d1,i) <- vs, (d2,j) <- vs, i < j ]
      checkOL (d1,d2) = checkOverlap am (Set.empty) d1 d2 
      checkAmb am env s =
          let derives = fromJust $ Map.lookup s am 
              dPairs  = distinctPairs derives
          in do { rs <- mapM checkOL dPairs 
                ; if or rs then 
                      addAmb s >> return True
                  else 
                      do { let ss = [ s | d <- derives, s <- allStatesDerive d, not (Set.member s env) ]
                         ; rs <- mapM (checkAmb am (Set.insert s env)) ss
                         ; return $ or rs }}
    

checkOverlap am env (DEps s1)    (DEps s2) = checkOverlapS am env s1 s2
checkOverlap am env (DCon c1 s1) (DCon c2 s2)
    | c1 == c2 =
        do { rs <- mapM (uncurry (checkOverlapS am env)) $ zip s1 s2
           ; return $ and rs }
    | otherwise = return False

checkOverlap am env DAny _ = return True
checkOverlap am env _ DAny = return True
checkOverlap am env _ _    = return False

checkOverlapS :: Ord s => Automaton s -> Set (s,s) -> s -> s -> State (CAConf s) Bool
checkOverlapS am env s1 s2 
    | Set.member (s1,s2) env || Set.member (s2,s1) env 
      = return False 
    | otherwise 
      = do { ol  <- getOLSet 
           ; uol <- getUnOLSet
           ; if Set.member (s1,s2) ol || Set.member (s2,s1) ol then 
                 return True
             else if Set.member (s1,s2) uol || Set.member (s2,s1) uol then 
                 return False
             else
                 let derives1 = fromJust $ Map.lookup s1 am 
                     derives2 = fromJust $ Map.lookup s2 am 
                     comb     = [ (d1,d2) | d1 <- derives1, d2 <- derives2 ]
                 in do { rs <- mapM (uncurry $ checkOverlap am (Set.insert (s1,s2) env)) comb
                       ; if all not rs then 
                             do { when (Set.null env) (addUnOL (s1,s2))
                                ; return False }
                         else
                             do { addOL (s1,s2)
                                ; return True } }}
                                       

