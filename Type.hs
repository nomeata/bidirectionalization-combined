module Type where

import AST 

import Data.Graph 
import Control.Monad.State
import Control.Monad.Error
import Util
import Data.Maybe
import Data.List (nub,nubBy,union)


import Data.Map (Map)
import qualified Data.Map as Map


-- type inference

initTMap :: [ (Name, FType) ]
initTMap =
    [ (Name "Z",   TFun [] [] (TCon (Name "Nat") [])),
      (Name "S",   TFun [] [TCon (Name "Nat") []] (TCon (Name "Nat") [])),
      (Name "Nil",  TFun [0] [] (TCon (Name "List") [TVar 0])),
      (Name "Cons", TFun [0] [TVar 0, TCon (Name "List") [TVar 0]] 
                (TCon (Name "List") [TVar 0])) ]


typeInference (AST decls) = 
    let mAst = do { (decls',_,_) <- 
                        foldr (\decls m -> 
                                   do (rdecls, tMap,  icount)  <- m
                                      (decls', tMap', icount') <- inferenceStep decls tMap icount
                                      return $ (decls'++rdecls, tMap', icount')
                              ) (return ([],initTMap,initIcount)) declss
                  ; return $ AST decls' } 
    in case mAst of 
         Left s  -> error s 
         Right a ->  a 
    where
      initIcount = 100 -- FIXME 
      declss = 
          let scc = stronglyConnComp callGraph 
          in  reverse $ map (\x -> case x of 
                           AcyclicSCC f  -> 
                               filter (\(Decl g _ _ _) -> f == g) decls
                           CyclicSCC  fs -> 
                               filter (\(Decl g _ _ _) -> g `elem` fs) decls) scc
--      callGraph = map (\f -> (f,f,snub $ f:funCallsE e)) $
--                     grupBy $ map (\(Decl f _ _ _) -> f) decls
      callGraph = 
          let fMap  = Map.fromListWith union $ 
                       map (\(Decl f _ _ e) -> (f,f:funCallsE e)) decls 
              fMap' = Map.map (snub) fMap 
          in map (\(f,fs) -> (f,f,fs)) $ Map.toList fMap'
      funCallsE (EVar _ _ v)    = []
      funCallsE (EFun _ _ f es) = f:concatMap funCallsE es 
      funCallsE (ECon _ _ _ es) = concatMap funCallsE es 


inferenceStep decls tmap icount = 
    let (decls0,  (tmpMap, icount0))  
            = runState (makeInitConstr tmap decls) ([],icount)
        (decls' , (constr, icount')) 
            = runState (mapM (assignTypeVars tmpMap tmap) decls0) ([],icount0)
    in
      do { (tmpMap', etypeMap') <- solveConstr tmpMap constr
         ; let decls'' = map (repl tmpMap' etypeMap') decls'
         ; return (decls'', tmpMap' ++ tmap, icount') }
        where 
          repl tM cM (Decl f ftype ps e) =
              Decl f (fromJust $ lookup f tM) (map replP ps) (replE e)
              where
                replP (PVar id (TVar i) v)    
                    = PVar id (fromJust $ lookup i cM) v
                replP (PCon id (TVar i) c ps)
                    = PCon id (fromJust $ lookup i cM) c (map replP ps)
                replE (EVar id (TVar i) v)
                    = EVar id (fromJust $ lookup i cM) v
                replE (ECon id (TVar i) c es)
                    = ECon id (fromJust $ lookup i cM) c (map replE es)
                replE (EFun id (TVar i) c es)
                    = EFun id (fromJust $ lookup i cM) c (map replE es)
          extractConstr ds = map (\(Decl f t _ _) -> (f,t)) $
                                nubBy isSameFunc ds



solveConstr tmpMap constr 
    = substStep constr (tmpMap, rearrange constr)
    where 
      introForAll (k,TFun _ ts t) =
          let vs = snub $ varsT t ++ concatMap varsT ts 
          in (k,TFun vs ts t)
      rearrange constr = 
          let vs = nub $ concatMap (\(t1,t2) -> varsT t1 ++ varsT t2) constr 
          in map (\x -> (x,TVar x)) vs                
      varsT (TVar i)    = [i]
      varsT (TCon _ ts) = concatMap varsT ts 
      varsT (TUndet)    = []
      substStep [] (tM,cM) = return (map introForAll tM,cM)
      substStep ((t,t'):cs) (tM,cM) =
          do { subs <- unify t t'
             ; substStep
                  (performSubstC subs cs)
                  (performSubstTM subs tM, performSubstCM subs cM) }
      performSubstC subs cs
          = map (\(t1,t2) -> (performSubstT subs t1, performSubstT subs t2)) cs
      performSubstTM subs tM 
          = map (\(k,v) -> (k, performSubstFT subs v)) tM
      performSubstCM subs cM
          = map (\(k,v) -> (k, performSubstT subs v)) cM
      performSubstFT subs (TFun is ts t) 
          = TFun [] (map (performSubstT subs) ts) (performSubstT subs t)
      performSubstT subs (TUndet) = TUndet 
      performSubstT subs (TVar i) = 
          case lookup (TVar i) subs of 
            Just t' -> t'
            _       -> TVar i
      performSubstT subs (TCon c ts) =
          TCon c (map (performSubstT subs) ts)
      unify :: Type -> Type -> Either String [ (Type, Type) ]
      unify (TVar i) t | not (i `elem` varsT t) = return [ (TVar i, t) ]
      unify t (TVar i) | not (i `elem` varsT t) = return [ (TVar i, t) ]
      unify (TVar i) (TVar j) | i == j = return []
      unify (TCon c ts) (TCon c' ts') | c == c' 
          = do { ss <- mapM (uncurry unify) $ zip ts ts'
               ; return $ concat ss }
      unify t t' = throwError $ "Can't unify types: " ++ show ( ppr (t,t'))
                 
    
               

makeInitConstr tmap decls =
    do { mapM_ (\(Decl f t ps e) ->
                      do { tmpMap <- getTmpMap 
                         ; case t of
                             FTUndet -> 
                                 case lookup f tmpMap of 
                                   Just t' -> 
                                       return ()
                                   _ -> 
                                       do { i  <- newTypeVar 
                                          ; is <- mapM (\_->newTypeVar) ps 
                                          ; let t' = TFun [] (map TVar is) (TVar i) 
                                          ; putTmpMap ((f,t'):tmpMap)
                                          ; return ()  }
                             _ -> 
                                 putTmpMap ((f,t):tmpMap)}) $ 
         (nubBy isSameFunc decls)
       ; tmpMap <- getTmpMap
       ; let decls' = map (\(Decl f t ps e) -> 
                             Decl f (fromJust $ lookup f tmpMap) ps e) decls
       ; return decls' }
    where getTmpMap    = do { (tmpMap,i) <- get; return tmpMap }
          putTmpMap tm = do { (_,i) <- get; put (tm,i) }
          newTypeVar   = do { (tm,i) <- get; put (tm,i+1); return i}

    
               

assignTypeVars tmpMap typeMap (Decl fname ftype ps e) =
    do ps' <- mapM assignTypeVarsP ps
       e'  <- assignTypeVarsE      e
       unifyFT ftype (TFun [] (map typeofP ps') (typeofE e'))
       let vtp = concatMap vtMapP ps'
       let vte = vtMapE e'
       mapM_ (\(x,t) -> case (lookup x vte) of 
                          { Just t' -> unifyT t t'; _ -> return ()}) vtp 
       mapM_ (\(x,t) -> case (lookup x vte) of 
                          { Just t' -> unifyT t t' }) vte 
       return $ Decl fname ftype ps' e'
    where
      vtMapP (PVar _ t x)    = [(x,t)]
      vtMapP (PCon _ _ c ps) = concatMap vtMapP ps 
      vtMapE (EVar _ t x)    = [(x,t)]
      vtMapE (ECon _ _ c es) = concatMap vtMapE es
      vtMapE (EFun _ _ c es) = concatMap vtMapE es
--      newTypeVar :: State ( [(Type,Type)], Int ) Int
      newTypeVar = do { (constr, icount) <- get
                      ; put (constr, icount+1)
                      ; return icount }
      addConstr s t = do { (constr, icount) <- get
                           ; put ((s,t):constr, icount) }
      assignTypeVarsP (PVar id t v) = 
          do { i <- newTypeVar
             ; unifyT t (TVar i) 
             ; return $ PVar id (TVar i) v } 
      assignTypeVarsP (PCon id t c ps) = 
          do { i <- newTypeVar
             ; case lookup c typeMap of
                 Just t' -> 
                     do { ps' <- mapM assignTypeVarsP ps 
                        ; unifyFT t' (TFun [] (map typeofP ps') (TVar i))
                        ; unifyT  t  (TVar i)
                        ; return $ PCon id (TVar i) c ps' }}
      assignTypeVarsE (EVar id t v) = 
          do { i <- newTypeVar 
             ; unifyT t (TVar i)
             ; return $ EVar id (TVar i) v }
      assignTypeVarsE (ECon id t c es) =
          do { i <- newTypeVar
             ; case lookup c typeMap of
                 Just t' -> 
                     do { es' <- mapM assignTypeVarsE es 
                        ; unifyFT t' (TFun [] (map typeofE es') (TVar i))
                        ; unifyT  t  (TVar i)
                        ; return $ ECon id (TVar i) c es' }}
      assignTypeVarsE (EFun id t f es) =
          do { i <- newTypeVar
             ; case lookup f (typeMap ++ tmpMap)  of
                 Just t' -> 
                     do { es' <- mapM assignTypeVarsE es 
                        ; unifyFT t' (TFun [] (map typeofE es') (TVar i))
                        ; unifyT  t  (TVar i)
                        ; return $ EFun id (TVar i) f es' }
                 _ ->
                     error $ (show f ++ " is not in " ++ show (typeMap ++ tmpMap))
             }
--      unifyT :: Type -> Type -> State ([(Type,Type)],Int) ()
      unifyT (TUndet) _ = return ()
      unifyT _ (TUndet) = return ()
      unifyT (TVar i) (TVar j) | i == j = return ()
      unifyT t t'       = addConstr t t'
      unifyFT (FTUndet) _ = return ()
      unifyFT _ (FTUndet) = return ()
      unifyFT t t' = 
          do { s  <- escapeForAll t 
             ; s' <- escapeForAll t'
             ; case (s,s') of 
                 (TFun _ ts t, TFun _ ts' t') ->
                     mapM_ (uncurry unifyT) $ zip (t:ts) (t':ts') }
      escapeForAll (TFun is ts t) =
          do { is' <- mapM (\_ -> newTypeVar) is 
             ; let ts' = map (replaceTVar (zip is is')) ts
             ; let t'   = replaceTVar (zip is is') t 
             ; return $ TFun [] ts' t'}
      replaceTVar table TUndet = TUndet
      replaceTVar table (TVar i) =
          case lookup i table of
            Just j -> TVar j 
            _      -> TVar i
      replaceTVar table (TCon t ts) =
          TCon t (map (replaceTVar table) ts)

                     
                          