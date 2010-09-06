module Shapify where

import Util 
import AST

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Function (fix)
import Data.Maybe
import Data.List ((\\), intersect)

-- replaces  "Nil :: Unit"            -> "Z"
--           "Cons(_,y) :: List Unit" ->  S(y)
introNat :: AST -> AST 
introNat (AST decls) =
    AST $ map introNatD decls 
    where
      natT = TCon (Name "Nat") []
      zP i   = PCon i natT (Name "Z") []
      sP i x = PCon i natT (Name "S") [x]
      zE i   = ECon i natT (Name "Z") []
      sE i x = ECon i natT (Name "S") [x]
      introNatD (Decl f (TFun is ts t) ps e) =
          let ts' = map replT ts 
              t'  = replT t
              ps' = map replP ps
              e'  = replE e
          in Decl f (TFun is ts' t') ps' e'
      replT (TCon (Name "List") [TCon (Name "Unit") []]) 
          = TCon (Name "Nat") []
      replT (TVar i)    = TVar i
      replT (TCon c ts) = TCon c (map replT ts)

      replP (PVar i (TCon (Name "List") [TCon (Name "Unit") []]) x)
          = PVar i (TCon (Name "Nat") []) x
      replP (PVar i t x) 
          = PVar i (replT t) x 
      replP (PCon i (TCon (Name "List") [TCon (Name "Unit") []]) 
                      (Name "Nil") [])
          = zP i
      replP (PCon i (TCon (Name "List") [TCon (Name "Unit") []])
                      (Name "Cons") [x,y])
          = sP i (replP y)
      replP (PCon i t c ps)
          = PCon i (replT t) c (map replP ps)
 
      replE (EVar i (TCon (Name "List") [TCon (Name "Unit") []]) x)
          = EVar i (TCon (Name "Nat") []) x
      replE (EVar i t x) 
          = EVar i (replT t) x 
      replE (ECon i (TCon (Name "List") [TCon (Name "Unit") []]) 
                      (Name "Nil") [])
          = zE i 
      replE (ECon i (TCon (Name "List") [TCon (Name "Unit") []])
                      (Name "Cons") [x,y])
          = sE i (replE y)
      replE (ECon i t c es) = ECon i (replT t) c (map replE es)
      replE (EFun i t f es) = EFun i (replT t) f (map replE es)
      

-- removes parameters/arguments of which type is Unit
-- FIXME: This function only checks "Unit" but no singleton types.
specializeUnit :: AST -> AST
specializeUnit (AST decls) =
    assignIDsAST $ AST $ map spUnitD decls 
    where
      spUnitD (Decl f (TFun is ts t) ps e) =
          let isUnits = map isUnit ts 
              ts'     = map fst $ filter (\(_,b) -> not b) $ zip ts isUnits 
              ps'     = map fst $ filter (\(_,b) -> not b) $ zip ps isUnits 
          in Decl f (TFun is ts' t) ps' (spUnitE e)
      spUnitE (EVar i t x)    = EVar i t x
      spUnitE (ECon i t c es) = ECon i t c (map spUnitE es)
      spUnitE (EFun i t f es) = 
          let isUnits = map isUnit (map typeofE es) 
              es'     = map fst $ filter (\(_,b) -> not b) $ 
                          zip (map spUnitE es) isUnits 
          in EFun i t f es' 
      isUnit (TCon (Name "Unit") []) = True
      isUnit _                       = False
              

-- replaces all "e::t" -> "Unit::Unit"
--          and "p::t" -> "Unit::Unit" 
-- in functions with type 
--              "forall ... t ... . ..."
shapify :: AST -> AST
shapify (AST decls) = --AST $ map shapifyD decls 
    specializeUnit $ 
     AST $ fix (\f proced pend rdecls -> 
                 let rest = pend \\ proced 
                 in if null rest then 
                        rdecls 
                    else
                        let decls' = shapifySig rest
                            pend'  = collectPending decls'
                        in f (rest++proced) pend' (decls'++rdecls))
            [] initPend []
    where
--       initPend = Map.toList $ Map.fromList $ map 
--                    (\(Decl f (TFun is ts t) _ _) -> 
--                         (f,TFun [] (map (replT is) ts) (replT is t))) decls
      initPend = Map.toList $ Map.fromList $ map
                  (\(Decl f (TFun is _ _) _ _) -> (f,is)) decls

      unitT = TCon (Name "Unit") []
      unitP i = PCon i unitT (Name "Unit") []
      unitE i = ECon i unitT (Name "Unit") []
              
      signituresMap 
          = Map.fromList $ map 
            (\(Decl f (TFun is ts t) _ _) -> (f,TFun is ts t)) decls

      collectPending decls =
          snub $ concatMap (\(Decl _ _ _ e) -> funCallsWithT e) decls
      funCallsWithT (EVar _ _ _)    = []
      funCallsWithT (ECon _ _ _ es) = concatMap funCallsWithT es
      funCallsWithT (EFun _ _ (IName f is) es) 
          = (Name f,is):concatMap funCallsWithT es 
      funCallsWithT (EFun _ _ _ es) = concatMap funCallsWithT es 
--       funCallsWithT (EFun t f es) = (f,TFun [] (map typeofE es) t):
--                                        concatMap funCallsWithT es
      shapifySig [] = []
      shapifySig ((f,is'):rs)
          = concatMap (\(d@(Decl g (TFun _ _ _) _ _))  ->
                           if f == g then 
                               [shapifyD is' d]
                           else
                               []
                           ) decls ++shapifySig rs 
      unifyU []                                   = []
      unifyU ((TCon (Name "Unit") [], TVar i):rs)    = i:unifyU rs
      unifyU ((TVar i, TCon (Name "Unit") []):rs)    = i:unifyU rs
      unifyU ((TCon c ts, TCon c' ts'):rs) | c == c' = unifyU (zip ts ts') ++ unifyU rs
      unifyU (_:rs)                                  = unifyU rs 
      shapifyD is' (Decl (Name s) (TFun is ts t) ps e)
          = let ftype = (TFun (is\\is') (map (replT is') ts) (replT is' t))
            in Decl (IName s is') ftype
                   (map (replP is') ps) (replE is' e)
      replT is (TVar i) | i `elem` is = unitT
                        | otherwise   = TVar i 
      replT is (TCon c ts) = TCon c (map (replT is) ts)

      replP is p = 
          case typeofP p of 
            TVar i | i `elem` is -> unitP (idofP p) 
            _ -> 
                case p of 
                  PVar i t x    -> PVar i (replT is t) x
                  PCon i t c ps -> PCon i (replT is t) c (map (replP is) ps)

      replE is e = 
          case typeofE e of 
            TVar i | i `elem` is -> unitE (idofE e)
            _ -> 
                case e of 
                  EVar i t x    -> EVar i (replT is t) x
                  ECon i t c es -> ECon i (replT is t) c (map (replE is) es)
                  EFun i t (IName f _) es ->
                      funCallE i is t f es                       
                  EFun i t (Name f) es ->
                      funCallE i is t f es
          where funCallE i is t f es = 
                    let es' = map (replE is) es 
                        TFun is' ts' t' 
                            = fromJust $ Map.lookup (Name f) signituresMap 
                        ts  = map typeofE es' 
                        is'' = intersect 
                                 is' 
                                 (snub $ unifyU $ zip (t:ts) (t':ts'))
                    in  
                      EFun i (replT is t) (IName f is'') es'
