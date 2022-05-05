---
title: El sistema de tipos de Haskell | 3/5
author: Guillaume Hoffmann
...

# Día 3

## Un par de cosas más sobre `Monad`

* la clase `Monad` tiene como superclase `Applicative` (provee `<*>`)
* `Applicative` tiene como superclase `Functor` (provee `fmap`, ie `<$>`)

## Transformadores de mónadas

A veces uno quiere combinar dos mónadas, por ejemplo `IO` con `Maybe`
o `IO` con `Either`, etc.

Para hacer eso se usan "monad transformers".

El transformador de mónadas WriterT (agrega la capacidad de loguear a una mónada):

<https://blog.infinitenegativeutility.com/2016/7/writer-monads-and-space-leaks>

Tutorial recomendado para explorar la combinación `IO` + `Either`:

<https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md>

## La librería `mtl`

Ahí se usan clases de tipos múltiples con functional dependencies.

Son clases de tipos (multiples) que generalizan monad transformers.

¿Qué son monad transformers?

La biblioteca `mtl` propone clases que generalizan patrones
de monad transformers:
<http://book.realworldhaskell.org/read/monad-transformers.htm>

## Plan

* multiclases
* functional dependencies

## Multi Parameters Type Classes

Generalización de las clases de tipos a más de un tipo

~~~haskell
class context => C a b where
  ...
~~~

. . .

~~~haskell
class context => C a b c where
  ...
~~~

## Ejemplo 1

~~~haskell
 {-# LANGUAGE MultiParamTypeClasses #-}

class Cast a b where
  cast :: a -> b
~~~

. . .

~~~haskell
instance Cast Int Float where
  cast = convertIntToFloat

instance Cast Int String where
  cast = show

instance Cast String Int where
  cast = read

instance Cast Char String where
  cast c = [c]
~~~

## Ejemplo real

~~~haskell
class (Real a, Fractional b) => RealToFrac a b where
    realToFrac :: a -> b

instance (Real a, Fractional a) => RealToFrac a a where
    realToFrac = id
~~~

Del package logfloat
<http://hackage.haskell.org/package/logfloat/docs/Data-Number-RealToFrac.html>

## Ejemplo

~~~haskell
class Eq e => Collection c e where
    insert :: c -> e -> c
    member :: c -> e -> Bool

instance Eq a => Collection [a] a where
    insert xs x = x:xs
    member = flip elem
~~~

## Ejemplo

~~~haskell
class Monad m => Store store m where
 new :: a -> m (store a)
 get :: store a -> m a
 put :: store a -> a -> m ()
~~~

. . .

~~~haskell
storeStrings :: (Store store m) => [String] -> m (store [String])
storeStrings xs = do
  store <- new []
  forM_ xs $ \x -> do   -- forM_ :: Monad m => [a] -> (a -> m b) -> m ()
    old <- get store
    put store (x : old)
  return store
~~~

. . .

~~~haskell
instance Store IORef IO where
  new = newIORef                            --    newIORef :: a -> IO (IORef a)
  get = readIORef                           --   readIORef :: IORef a -> IO a
  put ioref a = modifyIORef ioref (const a) -- modifyIORef :: IORef a -> (a -> a) -> IO () 

instance Store (STRef s) (ST s) where ...
~~~


## Problema con el ejemplo anterior

~~~haskell
ex ps = do
  store <- storeStrings ps
  get (store :: IORef [String])
~~~

¿Tipo de `ex`?

. . .

~~~haskell
> :t ex
ex :: (Store IORef m) => [Present] -> m [Present]
~~~

No menciona `IO`. ¿Debería?

## Problemas con MPTC

~~~haskell
class Eq e => Collection c e where
    insert :: c -> e -> c
    member :: c -> e -> Bool
~~~

~~~haskell
ins2 xs a b = insert ( insert xs a ) b
~~~

. . .

~~~haskell
> :t ins2
ins2 :: (Collection c e1, Collection c e2) => c -> e2 -> e1 -> c
~~~

. . .

Tiene solucion: especificar el tipo de `ins2`.

~~~haskell
ins2 ::  (Collection c e) => c -> e -> e -> c
~~~

Sorprendiente, pero no grave.

## Problema de tipos ambiguos

~~~haskell
class Eq e => Collection c e where
    insert :: c -> e -> c
    member :: c -> e -> Bool
~~~

Si le agregamos la funcion `empty :: c`, ¿qué pasa?

. . .

~~~haskell
error:
    • Could not deduce (Collection c e0)
      from the context: Collection c e
        bound by the type signature for:
                   empty :: forall c e. Collection c e => c
      The type variable ‘e0’ is ambiguous
    • In the ambiguity check for ‘empty’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      When checking the class method:
        empty :: forall c e. Collection c e => c
      In the class declaration for ‘Collection’
  |
  |     empty :: c
  |     ^^^^^^^^^^
~~~


## Intentemos arreglarlo

~~~haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

class Eq e => Collection c e where
    empty :: c

instance Collection [Int] Int where
   empty = []
~~~

En GHCi:

~~~haskell
> empty :: [Int]
~~~

¿Andará?

## ¡No!

~~~haskell
> empty :: [Int]

<interactive> error:
    • Ambiguous type variable ‘e0’ arising from a use of ‘empty’
      prevents the constraint ‘(Collection [Int] e0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘e0’ should be.
      These potential instance exist:
        instance [safe] Collection [Int] Int
    • In the expression: empty :: [Int]
      In an equation for ‘it’: it = empty :: [Int]
~~~

## Solución: Functional Dependencies

Una multiclase define una relación, pero a menudo,
lo que queremos realmente es una *función*. 

~~~haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Eq e => Collection c e | c -> e where
    empty  :: c
    insert :: c -> e -> c
    member :: c -> e -> Bool
~~~

## Functional Dependencies

~~~haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Monad m => Store store m | store -> m where
 new :: a -> m (store a)
 get :: store a -> m a
 put :: store a -> a -> m ()
~~~

. . .

~~~haskell
ex ps = do
  store <- storePresents ps
  get (store :: IORef [Present])
~~~

~~~haskell
> :t ex
ex :: [Present] -> IO [Present]
~~~


## Extensions vs. sistema de tipos por defecto

¿Porqué las multiclases y las funcional dependencies no son parte
del sistema de tipos por defecto de Haskell?

Respuesta de unos de los autores de GHC:

> Functional dependencies are very, very tricky.
> *Simon Peyton Jones*

<https://prime.haskell.org/wiki/MultiParamTypeClassesDilemma>

## Problemas con Functional Dependencies

* generacion de contradiciones
* la compilación puede no terminar

<https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf>

~~~haskell
class Mul a b c | a b -> c where
  mul :: a -> b -> c

type Vec b = [b]
instance Mul a b c => Mul a (Vec b) (Vec c) where
 mul a bs = map (mul a) bs

f b x y = if b then  mul  x [y] else y
~~~

Consecuencia:

~~~
error:
    • Reduction stack overflow; size = 201
      When simplifying the following type: Mul a0 [Vec c] (Vec c)
~~~

En general, funcional dependencies no son sound, complete y decidibles.

Habria que restringirlas.

## Pero.. ¡Functional Dependencies son divertidos!

Práctico: codificar enteros al nivel de los tipos (codificacion unaria)
y definir la suma y la multiplicación, listas y ordenamiento de listas:

Fun with functional dependencies (pdf)
<http://www.cse.chalmers.se/~hallgren/Papers/hallgren.pdf>

## Más cosas divertidas para explorar

* lógica propositional
  <https://typesandkinds.wordpress.com/2012/12/01/decidable-propositional-equality-in-haskell/>
* <https://byorgey.wordpress.com/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies/>


