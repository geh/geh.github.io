-- Define a class of mathematical rings
-- See also http://en.wikipedia.org/wiki/Ring_(mathematics)

module Ring where

 -- You can optionally include a list of symbols after an import statement
 -- to say exactly what you're importing from the other module. This is sometimes
 -- useful for documentation, and to avoid name clashes between modules.
import Control.Arrow ( first )
import Data.Maybe    ( listToMaybe )
import Data.List     ( stripPrefix )

-- Definition for rings
class Ring a where
  addId  :: a            -- additive identity
  addInv :: a -> a       -- additive inverse
  mulId  :: a            -- multiplicative identity

  add :: a -> a -> a     -- addition
  mul :: a -> a -> a     -- multiplication

-- The canonical instance for integers:
instance Ring Integer where
  addId  = 0
  addInv = negate
  mulId  = 1

  add = (+)
  mul = (*)

-- Numbers modulo 5:
data Mod5 = MkMod Integer
  deriving (Show, Eq)

unMod :: Mod5 -> Integer
unMod (MkMod n) = n

mkMod :: Integer -> Mod5
mkMod = MkMod . (`mod` 5)

instance Ring Mod5 where
  addId  = mkMod 0
  addInv = mkMod . negate . unMod
  mulId  = mkMod 1

  add x y = mkMod (unMod x + unMod y)
  mul x y = mkMod (unMod x * unMod y)

-- 2x2 matrices
data Mat2x2 = MkMat Integer Integer Integer Integer
  deriving (Show, Eq)

instance Ring Mat2x2 where
  addId  = MkMat 0 0 0 0
  addInv (MkMat a b c d) = MkMat (negate a) (negate b) (negate c) (negate d)
  mulId  = MkMat 1 0 0 1

  add (MkMat a1 b1 c1 d1) (MkMat a2 b2 c2 d2)
    = MkMat (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

  mul (MkMat a1 b1 c1 d1) (MkMat a2 b2 c2 d2)
    = MkMat (a1 * a2 + b1 * c2)
            (a1 * b2 + b1 * d2)
            (c1 * a2 + d1 * c2)
            (c1 * b2 + d1 * d2)

-- Boolean algebra
instance Ring Bool where
  addId  = False
  addInv = not
  mulId  = True

  add = (||)
  mul = (&&)

