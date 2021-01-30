import Data.Monoid ( (<>) )

newtype Suma = Suma Int deriving Show
newtype Prod = Prod Int deriving Show

{- definir instancias para Monoid
   luego probar en GHCi:

   *Main> (Suma 9) <> (Suma 100) <> (Suma 34)
   Suma 143
   *Main> (Prod 9) <> (Prod 100) <> (Prod 34)
   Prod 30600
-}

instance Monoid Suma where
  mempty = undefined
  mappend = undefined

instance Monoid Prod where
  mempty = undefined
  mappend = undefined
