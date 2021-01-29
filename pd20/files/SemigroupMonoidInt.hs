import Data.Semigroup
import Data.Monoid 

-- este módulo es para GHC 8.4 o ulterior
-- dado que en esas versiones, Monoid es una
-- subclase de Semigroup

newtype Suma = Suma Int deriving Show
newtype Prod = Prod Int deriving Show

instance Semigroup Suma where
  _ <> _ = undefined
 
instance Semigroup Prod where
  _ <> _ = undefined

instance Monoid Suma where
  mempty = undefined

instance Monoid Prod where
  mempty = undefined

{- definir instancias para Semigroup y Monoid,
   luego probar en GHCi que funcionan:

   *Main> (Suma 9) <> (Suma 100) <> (Suma 34)
   Suma 143
   *Main> (Prod 9) <> (Prod 100) <> (Prod 34)
   Prod 30600
-}



