{-# OPTIONS_GHC -Wall #-}
module Poly where

data Poly a = P [a]

-- Exercise 1 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) = undefined
 
-- Exercise 2 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show = undefined

-- Exercise 3 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 4 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 5 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

