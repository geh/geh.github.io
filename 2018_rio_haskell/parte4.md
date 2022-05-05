---
title: El sistema de tipos de Haskell | 4/5
author: Guillaume Hoffmann
...

# Día 4

## Plan

* Tipos existenciales y tipos fantasmas
* GADTs
* type families

## Type Families 

~~~haskell
{-# LANGUAGE TypeFamilies #-}
~~~

Una extensión que permite definir *funciones sobre tipos*.

Activa varias formas de definir estas:

* data families (que no vamos a ver)
* type synonym families

## Synonym families

3 posibilidades

* definidas al toplevel como familia abierta
* definidas al toplevel como familia cerrada
* apareciendo dentro de una clase de tipos

## Open type families

La anotación de kind es opcional y por defecto es `*`:

~~~haskell
type family Elem c :: *
~~~

~~~haskell
type instance Elem [e] = e
~~~

## Closed type families

Definiendo el conjunto completo de equaciones de la familia

~~~haskell
type family F a where
  F Int  = Double
  F Bool = Char
  F a    = String
~~~

En GHCi:

~~~haskell
*Main> :t (undefined :: F Int)
(undefined :: F Int) :: Double
*Main> :t (undefined :: F [a])
(undefined :: F [a]) :: String
~~~

## Ejemplos de type families

~~~haskell
type family F a :: *
type instance F [Int]   = Int   -- OK!
type instance F String  = Char  -- OK!
type instance F (F a)   = a     -- WRONG: type parameter mentions a type family
type instance
  F (forall a. (a, b))  = b     -- WRONG: a forall type appears in a type parameter
type instance
  F Float = forall a.a          -- WRONG: right-hand side may not be a forall type
type family H a where          -- OK!
  H Int  = Int
  H Bool = Bool
  H a    = String
type instance H Char = Char    -- WRONG: cannot have instances of closed family
type family K a where          -- OK!

type family G a b :: * -> *
type instance G Int            = (,)     -- WRONG: must be two type parameters
type instance G Int Char Float = Double  -- WRONG: must be two type parameters
~~~

## Restricciones

La reescritura bajo type families tiene reglas estrictas
para ser convergente:

1. Patrones de dos instancias no pueden solaparse:

~~~haskell
type instance F Int = Bool
type instance F Int = Char -- overlap prohibido
~~~

2. Dos instancias solapando de type families son permitidas si los lados derechos
   coinciden en la región del solapamiento:

~~~haskell
type instance F (a, Int) = [a]
type instance F (Int, b) = [b]   -- overlap permitted

type instance G (a, Int)  = [a]
type instance G (Char, a) = [a]  -- ILLEGAL overlap, as [Char] /= [Int]
~~~

## 

3. Los patrones deben ser "separados", según una noción fuerte de "separados":
   dos tipos son considerados como separados si los dos tipos no pueden ser unificados,
   incluso por un unificador potencialmente infinito.
   Esto prohibe el siguiente par de instancias

~~~haskell 
type instance H x   x = Int
type instance H [x] x = Bool
~~~

## Aplicación sobre tipos polimórficos

~~~haskell
type family F a where
  F Int = Bool  --
  F a   = Char  -- dos ecuaciones incompatibles

type family G a where 
  G Int = Int   --
  G a   = a     -- dos ecuaciones compatibles

> :t (undefined :: G a)
(undefined :: G a) :: a
> :t (undefined :: F Int)
(undefined :: F Int) :: Bool
> :t (undefined :: F [Int])
(undefined :: F [Int]) :: Char
> :t (undefined :: F a)
(undefined :: F a) :: a

<interactive> error:
    • Couldn't match expected type ‘F a’ with actual type ‘F a0’
      NB: ‘F’ is a type function, and may not be injective
      The type variable ‘a0’ is ambiguous
    • In the ambiguity check for an expression type signature
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In an expression type signature: F a
      In the expression: (undefined :: F a)
~~~

## Type families declaradas dentro de clases

~~~haskell
class Collects ce where
  type Elem ce :: *
  ...
~~~

* se puede omitir la palabra `family`
* los parametros deben ser todas variables
* algunos pueden ser parametros de la clase

## Instancias asociadas

~~~haskell
instance Eq (Elem [e]) => Collects [e] where
  type Elem [e] = e
  ...
~~~

* se puede omitir la palabra `instance`

Las instancias de tipos que se corresponden con los parametros de clases
deben tener precisamente el mismo tipo dado en la intancia de la clase:

~~~haskell
class Collects ce where
  type Elem ce :: *

instance Eq (Elem [e]) => Collects [e] where
  -- Choose one of the following alternatives:
  type Elem [e] = e       -- OK
  type Elem [x] = x       -- BAD; '[x]' is different to '[e]' from head
  type Elem x   = x       -- BAD; 'x' is different to '[e]'
  type Elem [Maybe x] = x -- BAD: '[Maybe x]' is different to '[e]'
~~~

## Injective Type Families

Caso donde la inferencia falla en presencia de type families:

~~~haskell
type family Id a
type instance Id Int = Int
type instance Id Bool = Bool

id :: Id t -> Id t
id x = x
~~~

Se rechaza la definición de `id` por que la variable `t` aparece
*solamente* bajo aplicaciones de familias de tipos, y es ambigua.

Con la extensión `TypeFamilyDependencies`:

~~~haskell
type family Id a = r | r -> a
~~~

## Type families para reemplazar Functional Dependencies

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
class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)
  get :: store a -> (StoreMonad store) a
  put :: store a -> a -> (StoreMonad store) ()
~~~

## Clases con TF e instancias

~~~haskell
class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)
  get :: store a -> (StoreMonad store) a
  put :: store a -> a -> (StoreMonad store) ()
~~~

. . .

~~~haskell
instance Store IORef where
  type StoreMonad IORef = IO
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

instance Store TVar where
  type StoreMonad TVar = STM
  new = newTVar
  get = readTVar
  put ioref a = modifyTVar ioref (const a)
~~~

##

~~~haskell
storeStrings :: (Store store, Monad (StoreMonad store))
              => [String] -> (StoreMonad store) (store [String])
storeStrings xs = do
  store <- new []
  forM_ xs $ \x -> do
    old <- get store
    put store (x : old)
  return store
~~~

## Anotaciones de igualdad de tipos en constraints:

~~~haskell
sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2
~~~

## Type families vs Functional Dependencies

Las type families proveen un estilo de programación sobre tipo
más *funcional* mientras que las dependencias funciones proveen
un estilo de programación *relacional*.

Además usar type families permite evitar de activar extensiones como:

* MultiParamTypeClasses
* FlexibleInstances

<https://wiki.haskell.org/Functional_dependencies_vs._type_families>

## Sintaxis `GADTSyntax`

La extensión `GADTSyntax` permite definir tipos de datos algebraicos
de la forma siguiente:

~~~haskell
data Dia where
    Lunes :: Dia
    Martes :: Dia
    ...
~~~

. . .

~~~haskell
data Maybe a where
    Nothing :: Maybe a
    Just    :: a -> Maybe a
~~~

. . .

~~~haskell
data Lista a where
    Nil :: Lista a
    Cons :: a -> Lista a -> Lista a
~~~

## Tipos existenciales

En la definición siguiente, la `x` es un tipo existencial:

~~~haskell
data Exists = forall x. Exists x
~~~

Usando el constructor `Exists`, el tipo `x` es escondido dentro del tipo `Exists`.
Podemos por ejemplo poner distintos  tipos en una lista:

~~~haskell
[Exists 1, Exists "hello", Exists 'a']
~~~

Pero luego se pierden esos tipos, no podemos hacer patten matching.
. . .

## Tipos existenciales

~~~haskell
data Exists = forall x. Exists x
~~~

¿Porqué el `forall`? Tiene sentido si lo vemos con la sintaxis `GADTSyntax`:

~~~haskell
data Exists where
  Exists :: x -> Exists
~~~

`Exists` es una función polimórfica, cualquier funcion polimórfica tiene `forall` implicitos:

~~~haskell
map :: forall a b . (a -> b) -> [a] -> [b]
~~~

Para poder escribir los `forall` explicitos activar la extensión
`ExplicitForAll`.

## RankNTypes

~~~haskell
mapExists :: (forall x. x -> x) -> Exists -> Exists
mapExists f (Exists x) = Exists (f x)
~~~

Esta función tiene polimorfismo de rango 2 (tiene un argumento
polimórfico). Requiere la extensión `RankNTypes` (implica `ExplicitForAll`).

## Tipos fantasmas

Un tipo fantasma es un tipo que no tiene un valor asociado, como
`phantom` en lo que sigue:

~~~haskell
data P phantom = P Int
~~~

La variable `phantom` no tiene un valor asociado a ella en la parte a
la derecha del símbolo igual. Esto significa que cuando construimos
un valor de tipo `P`, podemos también darle un tipo para `phantom`.
Como `phantom` no tiene un valor que le corresponde, es libre de
unificarse con cualquier cosa en el sistema de tipos.

Por ejemplo, cada uno de los siguientes casos es válido, incluso
dentro de un mismo programa:

~~~haskell
P 5 :: P String
P 5 :: P [Int]
P 5 :: P (IO ())
~~~

Podríamos imaginar estos ejemplos como formas de anotar el valor `P 5`.
En otros términos, una aplicación de los tipos fantasmas, es que nos permiten
embedir más información en nuestros tipos. En particular, queremos
adjuntar evidencia, o pruevas, a nuestros tipos. Es decir, queremos
asociar el tipo fantasma con un tipo testigo.

## Más control: `GADTs`

*Generalized* Algebraic Data Types:

~~~haskell
data Blob a where
    Bob :: a -> Blob a
    Bib :: Blob Int     -- esto es nuevo!
~~~

Ejemplo: definir un tipo "expresión" y un evaluador

<https://www.cis.upenn.edu/~sweirich/talks/GADT.pdf>

## Aplicación de los GADTs

Combinados con tipos fantasmas, se pueden usar para agregar
información al nivel de los tipos para garantizar propiedades:

~~~haskell
data Empty
data NonEmpty

data SafeList a b where
  Nil  :: SafeList a Empty
  Cons :: a -> SafeList a b -> SafeList a NonEmpty
~~~
. . .

~~~haskell
safeHead :: SafeList a NonEmpty -> a
~~~

¿Definición?

## Aplicación de Phantom Types, Existential Types y GADTSyntax a Darcs

Ver:

* tesis de Jason Dagit sobre garantizar
  propiedades del código de Darcs
  <http://darcs.net/Talks/DagitThesis>
* charla de David Roundy (FOSDEM 2006)
  <https://www.youtube.com/watch?v=hE88AR_9zIg>

## Más lectura sobre type families vs. functional dependencies

  * <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families>

Comparación type families vs. functional dependencies:

  * <https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html>
  * <https://ocharles.org.uk/blog/posts/2014-12-13-multi-param-type-classes.html>
  * <https://ocharles.org.uk/blog/posts/2014-12-14-functional-dependencies.html>
  * <https://dikgwahlapiso.wordpress.com/2015/08/30/moving-from-multiparameter-type-classes-and-functional-dependencies-to-type-families-in-haskell/>



