module Util where 

import Text.PrettyPrint
import Data.List (group, sort)

class Ppr a where
    ppr :: a -> Doc 
    pprList :: [a] -> Doc 
    pprList as = brackets (sep $ punctuate comma (map ppr as))

instance (Ppr Int) where 
    ppr i = int i 

instance (Ppr Integer) where
    ppr i = integer i 


instance (Ppr a, Ppr b) => Ppr (a,b) where
    ppr (a,b) = parens (sep $ punctuate comma [ppr a, ppr b])

instance (Ppr a, Ppr b, Ppr c) => Ppr (a,b,c) where 
    ppr (a,b,c) = parens (sep $ punctuate comma [ppr a, ppr b, ppr c])

instance Ppr a => Ppr [a] where
    ppr as    = pprList as -- brackets (sep $ punctuate comma (map ppr as))

instance Ppr Char where
    ppr c     = char c

instance (Ppr a, Ppr b) => Ppr (Either a b) where
    ppr (Left a)  = text "Left"  <+> ppr a 
    ppr (Right b) = text "Right" <+> ppr b

instance (Ppr a) => Ppr (Maybe a) where
    ppr (Nothing) = text "Nothing"
    ppr (Just a)  = text "Just" <+> ppr a 


{-# SPECIALIZE snub :: [Int] -> [Int] #-}
{-# SPECIALIZE snub :: [String] -> [String] #-}
snub :: Ord a => [a] -> [a]
snub = map head . group .  sort  
