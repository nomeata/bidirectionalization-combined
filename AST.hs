module AST where 

import Util 
import Text.PrettyPrint
import Data.List (groupBy)

import Control.Monad.State

data AST = AST [ Decl ]

instance Ppr AST where
    ppr (AST ds) = 
        let dss = groupBy (\(Decl f _ _ _) (Decl g _ _ _) -> f == g) ds
        in vcat $ map pprDecls dss 
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
        addSig (ppr fname <> 
                     parens (hsep $ punctuate comma (map ppr ps)) $$
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
      _      -> d <> text "::" <> ppr t 

instance Ppr Exp where
    ppr (EVar _ t vname)     
        = addPprType t (ppr vname)
    ppr (EFun _ t fname es) 
        = addPprType t $
            ppr fname <>
            parens (sep $ punctuate comma (map ppr es))          
    ppr (ECon _ t cname []) 
        = addPprType t $ ppr cname 
    ppr (ECon _ t cname es)
        = addPprType t $ 
             ppr cname <>
             parens (sep $ punctuate comma (map ppr es))

instance Show Exp where
    show = show . ppr 

data Pat  = PVar ID Type Name
          | PCon ID Type Name [Pat] 
       deriving (Ord,Eq)

instance Ppr Pat where
    ppr (PVar _ t vname)     
        = addPprType t (ppr vname)
    ppr (PCon _ t cname [])
        = addPprType t (ppr cname)
    ppr (PCon _ t cname ps)
        = addPprType t $ ppr cname 
          <> parens (sep $ punctuate comma (map ppr ps))
          
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

instance Ppr TAST where 
    ppr (TAST tdecls) = vcat $ map ppr tdecls 
instance Ppr TDecl where
    ppr (TDecl f ps es vdecls) =
        ppr f <> parens (hsep $ punctuate comma (map ppr ps)) $$
            nest 4 (text "=" <+> parens (hsep $ punctuate comma (map ppr es))) $$
            if null vdecls then 
                empty 
            else 
                (nest 6 (text "where") $$
                 nest 8 (vcat $ map ppr vdecls))
instance Ppr VDecl where
    ppr (VDecl vs f us) = parens (hsep $ punctuate comma (map ppr vs))
                          <+> text "=" <+> ppr f <>
                          parens (hsep $ punctuate comma (map ppr us))
