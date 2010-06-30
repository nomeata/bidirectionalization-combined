{-# OPTIONS -XMultiParamTypeClasses -XTemplateHaskell #-}

module CodeGen where

import qualified Language.Haskell.TH as TH

import Text.PrettyPrint 
import Debug.Trace
import Data.List (groupBy)

import AST
import Util

generateCodeBwd :: (AST, AST, AST, TAST) -> [ TH.Dec ]
generateCodeBwd (orig, bwd, cmpl, tinv) = 
    convCmpl orig ++ convBWD bwd ++ convCmpl cmpl ++ convNDet tinv 

convP (PCon _ _ (Name "Cons") [p1,p2]) = 
    TH.InfixP (convP p1) (TH.mkName ":") (convP p2)
convP (PCon _ _ (Name "Nil") []) =
    TH.ListP []
convP (PCon _ _ c cs) = 
    TH.ConP (TH.mkName $ show c) $ map convP cs
convP (PVar _ _ v)    = TH.VarP (TH.mkName $ show v) 

returnE e     = TH.AppE (TH.VarE $ TH.mkName "return") e
mplusE  e1 e2 = TH.AppE (TH.AppE (TH.VarE $ TH.mkName "mplus") e1) e2 
mzeroE        = TH.VarE $ TH.mkName "mzero"

name :: Show a => a -> TH.Name
name  = TH.mkName . show 
nameE = TH.VarE . name 

apply f es = foldl TH.AppE f es

convBWD (AST decls) = map convBWDD decls 
    where
      convBWDD (Decl f _ ps e) = TH.FunD (TH.mkName $ show f)
                                 [ TH.Clause (map convP ps) (TH.NormalB $ convE e) [] ]
      convE (EFun _ _ (NInv f) [EVar _ _ v]) = 
          TH.AppE (TH.VarE $ TH.mkName "head") $ 
            TH.AppE (nameE (NInv f)) (nameE v)
      convE (EFun _ _ (NInv (NTuple f)) 
                      [EVar _ _ v, EFun _ _ (NCmpl _) [EVar _ _ s]]) = 
          TH.AppE (TH.VarE $ TH.mkName "head") $ 
             apply (nameE (NInv (NTuple f))) 
                       [nameE v, TH.AppE (nameE (NCmpl f)) (nameE s)]

convCmpl (AST decls) = map convCmplF $ groupBy isSameFunc decls
    where 
      convCmplF (ds@(Decl f _ _ _:_)) =
          TH.FunD (name f) $ map convCmplD ds 
      convCmplD (Decl _ _ ps e) = TH.Clause [TH.TupP $ map convP ps] (TH.NormalB $ convE e) []
      convE (EVar _ _ v)    = nameE v
      convE (ECon _ _ (Name "Cons") [e1,e2]) = TH.InfixE (Just $ convE e1) (TH.VarE $ TH.mkName ":") (Just $ convE e2)
      convE (ECon _ _ (Name "Nil")  [])      = TH.ListE []
      convE (ECon _ _ c es) = apply (TH.ConE (name c)) $ map convE es 
      convE (EFun _ _ f es) = apply (TH.VarE (name f)) $ [TH.TupE $ map convE es ]


convE (EVar _ _ v)    = nameE v
convE (ECon _ _ (Name "Cons") [e1,e2]) = TH.InfixE (Just $ convE e1) (TH.VarE $ TH.mkName ":") (Just $ convE e2)
convE (ECon _ _ (Name "Nil")  [])      = TH.ListE []
convE (ECon _ _ c es) = apply (TH.ConE (name c)) $ map convE es 
convE (EFun _ _ f es) = apply (TH.VarE (name f)) $ map convE es 


convNDet (TAST tdecls) 
    = concatMap convNDetF $ groupBy isSameFuncT tdecls 
    where
      isSameFuncT (TDecl f _ _ _) (TDecl g _ _ _) = f == g 
      vars    = [ TH.mkName $ "x"    ++ show i        | i <- [1..] ]
      funcs f = [ TH.mkName $ show f ++ "_" ++ show i | i <- [1..] ] -- name f = TH.mkName $ show f 
      convNDetF (ds@(TDecl f ps _ _:_)) = -- NInj
          [ TH.FunD (name f) 
            [ TH.Clause (map TH.VarP $ take (length ps) vars)
                        (TH.NormalB mpluses)
                        [] ] ]
          ++ zipWith convNDetD ds (funcs f)
          where
            mpluses = foldr mplusE mzeroE $
                        map (\f -> apply (TH.VarE f) $ map TH.VarE (take (length ps) vars) ) $
                            take (length ds) (funcs f)
      convNDetD (TDecl _ ps es vs) f =
          TH.FunD f [ TH.Clause (map convP ps)         (TH.NormalB $ convEs es vs) [],
                      TH.Clause [ TH.WildP | _ <- ps]  (TH.NormalB mzeroE) [] ]
      
      convEs es vs = TH.DoE $ (map mkBind vs) ++ [ TH.NoBindS (returnE $ TH.TupE $ map convE es) ]
          where
            mkBind (VDecl us f vs) = TH.BindS (TH.TupP $ map (TH.VarP . name) us)
                                           (apply (nameE f) $ map (TH.VarE . name) vs)
                                           
      

instance Ppr TH.Dec where
    ppr = text . show . TH.ppr
    pprList vs = vcat $ map ppr vs 