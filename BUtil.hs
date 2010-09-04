{-# OPTIONS -XRank2Types -XCPP -XScopedTypeVariables #-}
module BUtil where

import qualified Data.IntMap as IntMap 
import Control.Monad 

import System.IO.Unsafe

import Control.Exception

data Nat = S Nat | Z deriving (Eq)

instance Show Nat where
  show = show . fromNat

instance Num Nat where
  (+) = error "No operators defined for Nat"
  (*) = error "No operators defined for Nat"
  abs = error "No operators defined for Nat"
  signum = error "No operators defined for Nat"
  fromInteger n | n < 0  = error "Nat cannot be negative"
                | n >= 0 = toNat n

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
#if __GLASGOW_HASKELL__ >= 610 
           Left (e::SomeException) -> return $ Nothing 
#else
           Left  e -> return $ Nothing 
#endif
           Right r -> return $ Just $ r }

type Bias = Int -> [ Int ]
rear l    = [ 0 .. l - 1 ]
front l   = reverse [ 0 .. l - 1 ]
middle l  = [1,3..l] + (reverse [2,4..l])
borders l = (reverse [1,3..l])+[2,4..l]
