module AST where 

import Util 
import Text.PrettyPrint
import Data.List (groupBy)

import Debug.Trace 
import Control.Monad.State

data AST = AST [ Decl ]

instance Ppr AST where
    ppr (AST ds) = 
        let dss = groupBy isSameFunc ds
        in vcat $ punctuate (text "\n") $ map pprDecls dss 
        where 
          pprDecls (d:ds) =
              ppr d $$ pprDeclsSimp ds 
          pprDeclsSimp ds
              = vcat $ map (\(Decl f _ ps e) -> ppr (Decl f FTUndet ps e)) ds

instance Show AST where 
    show = show . ppr 

data Name 
    = Name     String 
    | IName    String [Int]
    | NCmpl    Name 
    | NTuple   Name 
    | NTupleV  Int      -- Variable Introduced by Tupling 
    | NTupleVC Int      -- Variable Introduced by Tupling 
    | NBwd     Name
    | NInv     Name 
    | NCConF   Name     -- Constructor Name in Complement (1)
    | NCConE   Int      -- Constructor Name in Complement (2)
    | NCConU   Name Int -- After Renaming  
          deriving (Eq,Ord)

instance Ppr Name where
    ppr  (Name s) = text s
    ppr  (IName s is) = text s <> text "_" <> 
                        (hcat $ punctuate (text "_") $ map ppr is)
    ppr  (NCmpl n)  = ppr n <> text "_Cmpl" 
    ppr  (NTuple n) = ppr n <> text "_T"  
    ppr  (NTupleV i)  = text "tv" <> ppr i
    ppr  (NTupleVC i) = text "tc" <> ppr i
    ppr  (NInv n)     = ppr n <> text "_I" 
    ppr  (NBwd n)     = ppr n <> text "_B"
    ppr  (NCConF n)   = text "C" <> ppr n
    ppr  (NCConE i)   = text "C" <> ppr i 
    ppr  (NCConU n i) = text "C" <> ppr n <> text "_" <> ppr i

instance Show Name where
    show = show . ppr 

data Decl = Decl Name FType [Pat] Exp

instance Ppr Decl where
    ppr (Decl fname ftype ps e) = 
        addSig (ppr fname <+> 
                    (hsep $ map pprChildP ps) $$
--                     parens (hsep $ punctuate comma (map ppr ps)) $$
                     nest 4 (text "=") $$
                     nest 6 (ppr e))
        where 
          addSig d = 
              case ftype of
                FTUndet -> empty <> d
                _       -> ppr fname <+> text "::" <+> ppr ftype $$ d            
instance Show Decl where
    show = show . ppr 

data Exp  = EVar ID Type Name 
          | EFun ID Type Name [Exp] -- Exp must be variable (treeless)
          | ECon ID Type Name [Exp]
       deriving (Ord,Eq)

type ID = Maybe Int

addPprType t d = 
    case t of 
      TUndet -> d
      _      -> parens ( d <> text "::" <> ppr t )


pprChildE e | isAtomicE e = ppr e 
            | otherwise   = parens (ppr e)
isAtomicE (EVar _ _ _)     = True
isAtomicE (EFun _ _ _ [])  = True
isAtomicE (ECon _ _ _ [])  = True
isAtomicE e | isAllListE e = True 
isAtomicE _                = False

pprListE (ECon _ _ (Name "Cons") [e1,ECon _ _ (Name "Nil") []]) 
    = ppr e1 
pprListE (ECon _ _ (Name "Cons") [e1,e2]) 
    = ppr e1 <> comma <+> pprListE e2 
pprListE (ECon _ _ (Name "Nil") []) 
    = empty

isAllListE (ECon _ _ (Name "Cons") [e1,e2])
    = isAllListE e2 
isAllListE (ECon _ _ (Name "Nil") []) 
    = True
isAllListE _ 
    = False 

instance Ppr Exp where
    ppr (EVar _ t vname)     
        = addPprType t (ppr vname)
    ppr (EFun _ t fname es) 
        = addPprType t $
            ppr fname <+>
                (hsep $ map pprChildE es)
--            parens (sep $ punctuate comma (map ppr es))          
    ppr e | isAllListE e
        = brackets (pprListE e )
    ppr (ECon _ _ (Name "Cons") [e1,e2]) 
        = pprChildE e1 <> text ":" <> ppr e2 
    ppr (ECon _ _ (Name "Unit") []) 
        = parens empty 
    ppr (ECon _ t cname []) 
        = addPprType t $ ppr cname 
    ppr (ECon _ t cname es)
        = addPprType t $ 
             ppr cname <+>
                 (hsep $ map pprChildE es)
--             parens (sep $ punctuate comma (map ppr es))


instance Show Exp where
    show = show . ppr 

data Pat  = PVar ID Type Name
          | PCon ID Type Name [Pat] 
       deriving (Ord,Eq)


pprChildP p | isAtomicP p = ppr p
            | otherwise   = parens (ppr p)
isAtomicP (PVar _ _ _)     = True
isAtomicP (PCon _ _ _ [])  = True
isAtomicP p | isAllListP p = True
isAtomicP _                = False

pprListP (PCon _ _ (Name "Cons") [p1, PCon _ _ (Name "Nil") []])
    = ppr p1
pprListP (PCon _ _ (Name "Cons") [p1,p2]) 
    = ppr p1 <> comma <+> pprListP p2 
pprListP (PCon _ _ (Name "Nil") []) 
    = empty

isAllListP (PCon _ _ (Name "Cons") [p1,p2])
    = isAllListP p2 
isAllListP (PCon _ _ (Name "Nil") []) 
    = True
isAllListP _ 
    = False 


instance Ppr Pat where
    ppr (PVar _ t vname)     
        = addPprType t (ppr vname)
    ppr e | isAllListP e
        = brackets (pprListP e )
    ppr (PCon _ _ (Name "Cons") [p1,p2]) 
        = pprChildP p1 <> text ":" <> ppr p2 
    ppr (PCon _ _ (Name "Unit") []) -- never happens 
        = parens empty 
    ppr (PCon _ t cname [])
        = addPprType t (ppr cname)
    ppr (PCon _ t cname ps)
        = addPprType t $ ppr cname 
           <+> (hsep $ map pprChildP ps)
--           <> parens (sep $ punctuate comma (map ppr ps))
          
instance Show Pat where
    show = show . ppr 

data FType 
    = TFun [Int] [Type] Type -- Quantified Vars, Input Types, Output Type.
      -- e.g. forall a. [a] -> a 
      --      ==> TFun [TVar 1] [ TCon "[]" [TVar 1] ] (TVar 1)
    | FTUndet
      deriving (Eq,Ord)

instance Ppr FType where
    ppr (FTUndet) = text "??"
    ppr (TFun is ts t) =
        (case is of 
          [] -> empty 
          _  -> text "forall" <+>
                hsep (map pprTV is) <>
                text ".")
         <+> argType <+>
             ppr t 
        where
          argType = 
              case ts of 
                []  -> empty 
                [t] -> ppr t <+> text "->" 
                _   -> 
                    parens (sep $ punctuate comma (map ppr ts)) 
                               <+> text "->"
          pprTV i = text ("t" ++ show i)
             
instance Show FType where
    show = show . ppr 

data Type = TUndet           -- PlaceHolder
          | TVar Int         -- Type Variable
          | TCon Name [Type] -- e.g. [Int], Map Int [Char]
            deriving (Eq,Ord)
            
instance Ppr Type where
    ppr (TUndet)        = text "?"
    ppr (TVar i)        = text ("t" ++ show i) 
    ppr (TCon (Name "Unit") []) =
        parens empty 
    ppr (TCon (Name "List") [t]) = 
        brackets $ ppr t
    ppr (TCon tname ts) =
        ppr tname <+> 
             hsep (map (\t -> f t (ppr t)) ts)
        where
          f (TUndet) x = x
          f (TVar i) x = x 
          f (TCon tname []) x = x
          f _ x = parens x
instance Show Type where 
    show = show . ppr 



typeofP (PVar _ t _)   = t 
typeofP (PCon _ t _ _) = t 
typeofE (EVar _ t _)   = t 
typeofE (ECon _ t _ _) = t
typeofE (EFun _ t _ _) = t 


idofP (PVar t _ _)   = t 
idofP (PCon t _ _ _) = t 
idofE (EVar t _ _)   = t 
idofE (ECon t _ _ _) = t
idofE (EFun t _ _ _) = t 

varsP p = snub $ vp p
    where vp (PVar _ _ x)    = [x]
          vp (PCon _ _ _ ps) = concatMap vp ps
varsE e = snub $ ve e 
    where ve (EVar _ _ x)    = [x]
          ve (ECon _ _ _ es) = concatMap ve es
          ve (EFun _ _ _ es) = concatMap ve es


-- assignIDsAST :: AST -> AST
assignIDsAST (AST decls) = 
    AST $ evalState (mapM assignIDsD decls) 10

assignIDsD :: Decl -> State Int (Decl) 
assignIDsD (Decl f t ps e) = 
    do { ps' <- mapM assignIDsP ps 
       ; e   <- assignIDsE e 
       ; return $ Decl f t ps e }
    where
      uniq = do { i <- get; put (i+1) ; return $ Just (i+1) }
      assignIDsE (EVar _ t x) = 
          do { i <- uniq
             ; return $ EVar i t x }
      assignIDsE (ECon _ t c es) = 
          do { i <- uniq 
             ; es' <- mapM assignIDsE es
             ; return $ ECon i t c es' }
      assignIDsE (EFun _ t f es) = 
          do { i <- uniq 
             ; es' <- mapM assignIDsE es
             ; return $ EFun i t f es' }

      assignIDsP (PVar _ t x) =
          do { i <- uniq
             ; return $ PVar i t x }
      assignIDsP (PCon _ t c ps) = 
          do { i <- uniq
             ; ps' <- mapM assignIDsP ps 
             ; return $ PCon i t c ps' }

isSameFunc (Decl f _ _ _) (Decl g _ _ _) = f == g 


-- After Tupling

data TAST  = TAST [TDecl]
data TDecl = TDecl Name [Pat] [Exp] [VDecl] -- f(ps) = es where ... 
data VDecl = VDecl [Name] Name [Name]          -- vs = f(us)

parensIfMultiple []  = parens empty 
parensIfMultiple [p] = p
parensIfMultiple ps  = parens (hsep $ punctuate comma ps)

instance Ppr TAST where 
    ppr (TAST tdecls) = 
        let tdeclss = groupBy (\(TDecl f _ _ _) (TDecl g _ _ _) -> f == g) tdecls 
        in vcat $ punctuate (text "\n") $ map (\tdecls -> vcat $ map ppr tdecls) tdeclss


instance Ppr TDecl where
    ppr (TDecl f ps es vdecls) =
        ppr f <+> parensIfMultiple (map ppr ps) $$
            nest 4 (text "=" <+> parensIfMultiple (map ppr es)) $$
            if null vdecls then 
                empty 
            else 
                (nest 6 (text "where") $$
                 nest 8 (vcat $ map ppr vdecls))
instance Ppr VDecl where
    ppr (VDecl vs f us) = parensIfMultiple (map ppr vs)
                          <+> text "=" <+> ppr f <>
                          parensIfMultiple (map ppr us)
