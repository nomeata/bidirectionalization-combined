{-# OPTIONS -XRank2Types -XCPP #-}
module BUtil where

import qualified Data.IntMap as IntMap 
import Control.Monad 

import System.IO.Unsafe

#if __GLASGOW_HASKELL__ >= 610 
import Control.OldException
#else
import Control.Exception
#endif

data Nat = S Nat | Z deriving (Show,Eq)

toNat x = if x == 0 then 
              Z
          else 
              S (toNat $ x-1)

fromNat Z     = 0
fromNat (S x) = 1 + fromNat x

fromDistinctList = IntMap.fromList 

gen_put_bias :: Bias 
                -> (forall a. [a] -> [a]) 
                -> (Nat -> Nat -> Maybe Nat) 
                -> [a] -> [a] 
                -> Maybe [Maybe a]
gen_put_bias bias get sput s v =
    do { let ls = length s  
       ; let g = fromDistinctList (zip (bias ls) s)
       ; l' <- maybe (fail "...")
                     return
                     (sput (toNat ls) (toNat (length v)))
       ; let t = bias (fromNat l')
       ; let h = fromDistinctList (zip (get t) v)
       ; let h'= IntMap.union h g 
       ; return (map (flip IntMap.lookup h') t) }

withDefaultBias put bias d s v =
    do { s' <- put bias s v 
       ; return (map (maybe d id) s') }

withDefault put d s v =
    do { s' <- put s v 
       ; return (map (maybe d id) s') }

gen_put_dbias :: Bias -> (forall a. [a] -> [a]) 
                 -> (Nat -> Nat -> Maybe Nat)
                 -> a -> [a] -> [a] -> Maybe [a]
gen_put_dbias bias get sput d s v =
    do { s' <- gen_put_bias bias get sput s v
       ; return (map (maybe d id) s') }

castError :: a -> Maybe a 
castError f = unsafePerformIO $ 
    do { r <- try (evaluate f)
       ; case r of
           Left  e -> return $ Nothing 
           Right r -> return $ Just $ r }

type Bias = Int -> [ Int ]
rear l    = [ 0 .. l - 1 ]
front l   = reverse [ 0 .. l - 1 ]
middle l  = [1,3..l] + (reverse [2,4..l])
borders l = (reverse [1,3..l])+[2,4..l]
