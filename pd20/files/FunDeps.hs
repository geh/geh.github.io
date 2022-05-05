{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module FunDeps where

data Zero
data Succ n

type Three = Succ ( Succ ( Succ Zero ) )

data True
data False

class Even n b | n -> b where
  even :: n -> b

class Odd n b  | n -> b where
  odd :: n -> b

instance Even  Zero  True
instance Odd   Zero  False

instance Odd n b  =>  Even (Succ n) b
instance Even n b =>  Odd  (Succ n) b


class Add a b c | a b -> c where
  add :: a -> b -> c

instance              Add Zero b b
instance Add a b c => Add (Succ a) b (Succ c)

class Mul a b c | a b -> c where
  mul :: a -> b -> c

instance                           Mul Zero b Zero
instance (Mul a b c, Add b c d) => Mul (Succ a) b d

u :: a
u = undefined

type One = Succ Zero

class Pow a b c | a b -> c where
  pow :: a -> b -> c

instance                           Pow a Zero One
instance (Pow a b c, Mul a c d) => Pow a (Succ b) d


