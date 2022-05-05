{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}


import qualified Data.Set as S

module Multi where

class Eq e => Collection c e where
    insert :: c -> e -> c
    member :: c -> e -> Bool

instance Eq a => Collection [a] a where
    insert xs x = x:xs
    member = flip elem
