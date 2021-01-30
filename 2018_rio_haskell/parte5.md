---
title: El sistema de tipos de Haskell | 5/5
author: Guillaume Hoffmann
...

# Día 5

## Plan

* Kinds otra vez
* DataKinds
* Un poco de TypeInType

## Cuando los Kinds no bastan

> Recent innovations in Haskell
> have been moving in exactly this direction. Notably, GADTs [24]
> and type families [25] turn the type system into a (modest) pro-
> gramming language in its own right.
>
> But, embarrassingly, type-level programming in Haskell is
> almost entirely untyped
> because the kind system has too few kinds (`*`, `* -> *`, and so on).
>
> *Giving Haskell a Promotion, 2012* <http://dreixel.net/research/pdf/ghp.pdf> 

## Volviendo a los GADTs

~~~haskell
data Zero
data Succ n

data Vec :: * -> * -> * where
  Nil    :: Vec a Zero
  Cons   :: a -> Vec a n -> Vec a (Succ n)

safeHead :: Vec a (Succ n) -> a
safeHead (Cons h _) = h
~~~

* dentro del sistema de tipos, el tipo (kind) de `Succ` es `*`.
* no da indicación sobre para qué se supone que sirva
* podríamos escribir `Succ Bool` o `Vec Zero Int`

##

Mejor sería poder escribir:

~~~haskell
data Nat = Zero | Succ Nat

data Vec :: * -> Nat -> * where
  VNil    :: Vec a Zero
  VCons   :: a -> Vec a n -> Vec a (Succ n)
~~~

* El segundo argumento de `Vec` sería un *valor* de tipo `Nat`.
* `Vec Zero Int` daría un error de tipeo

¡La extensión `DataKinds` habilita esto!

## DataKinds y los nombres ambiguos

A menudo se define un tipo con un constructor del mismo nombre: `data T = T Int`.

En caso de ambiguedad de usa `'T` para el tipo promovido en kind por DataKinds:

~~~haskell
data Nat = Zero | Succ Nat

data Vec :: * -> Nat -> * where
  VNil    :: Vec a 'Zero
  VCons   :: a -> Vec a n -> Vec a ('Succ n)
~~~

Si no hay ambiguedad, el apóstrofe puede ser omitido.

Al nivel de los kinds el apóstrofe no es necesario (ni permitio) porque
no hay ambiguedad posible.

## Polimorfismo de kinds para tipos promovidos

También se promueven tipos de datos parametrizados:

~~~haskell
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList as -> HList (a:as)
~~~

Esto declara el tipo `HList` de las listas *heterogeneas*.

`HList` representa una lista de tipos que almacena los tipos
de los elementos, por ejemplo:

~~~haskell
HCons "Hi" (HCons True HNil) :: HList (String: Bool : '[])
~~~

es una lista heterogenea con dos valores de tipos distintos.

##

Haskell permite la sintaxis `[a,b]` para la lista explícita `(a:b:[])`,
y tambien esto se extiende a la sintaxis de los tipos, entonces:

~~~haskell
HCons "Hi" (HCons True HNil) :: HList '[String,Bool]
~~~

Observar la apóstrofe para distinguir la lista al nivel de los tipos,
del tipo "lista" en, por ejemplo, `reverse :: [a] -> [a]`.


##

¿Cuál es el tipo del constructor de datos promovido `'(:)`?

Como el constructor de datos `(:)` es polimórfico sobre tipos,
el constructor promovido `'(:)` debe ser polimórfico sobre kinds:

~~~haskell
'(:) :: forall k . k -> [k] -> [k]
~~~

donde `k` una variable de kinds.

## Restricciones de `DataKinds` 1

Con `DataKinds`, GHC promueve automáticamente cada tipo de datos
a un kind, y sus constructores (de valores) a constructores de tipos:

~~~haskell
data Nat = Zero | Succ Nat
data List a = Nil | Cons a (List a)
data Pair a b = Pair a b
data Sum a b = L a | R b
~~~

~~~
Nat :: *
'Zero :: Nat
'Succ :: Nat -> Nat

List :: * -> *
'Nil  :: forall k. List k
'Cons :: forall k. k -> List k -> List k

Pair  :: * -> * -> *
'Pair :: forall k1 k2. k1 -> k2 -> Pair k1 k2

Sum :: * -> * -> *
'L :: k1 -> Sum k1 k2
'R :: k2 -> Sum k1 k2
~~~


## Restricciones de `DataKinds` 2

La promoción tiene las siguientes restricciones:

* se promueven `data` y `newtype` pero no sinónimos de tipos,
  tampoco type families.

* solo se promueven tipos que tienen kinds de la forma
  `* -> ... -> * -> *`.
  En particular, no se promueven tipos con kinds de alto orden
  como `data Fix f = In (f (Fix f))`,
  o tipos de datos cuyos kinds involucran a tipos promovidos,
  como `Vec :: * -> Nat -> *`.

* tampoco se promueven constructores de datos que tienen kinds
  polimórficos, involucran restricciones de clases de tipos
  (`data T x = Ord x => T x`), mencionan type families o involucran
  tipos no promocionables

## Averiguar kinds en GHCi

Solo los tipos cuyos kind es `*` pueden ser habitados (= tener valores).
Por lo cual no se puede hacer lo siguiente en GHCi:

~~~haskell
> :t (undefined :: Maybe) -- Maybe :: * -> *
> :t (undefined :: 'S 'Z) -- 'S 'Z :: Nat
~~~

Pero se puede directamente pedir el kind de estos tipos:

~~~
> :kind Num Int
Num Int :: Constraint
> :kind 'S 'Z
'S 'Z :: Nat
~~~

Para forzar la evaluación de las funciones sobre tipos (type families)
usar el comando `:kind!` :

~~~
> :kind! ('S 'Z :+ 'S 'Z)
'S ('S 'Z) :: Nat
~~~

## `TypeInType`

La extensión `TypeInType` suelta algunas restricciones:

* promoción de sinónimos de tipos y familias de tipos, pero
  no de data families (el sistema de tipos de GHC no soportaría
  lo último)
* todos los tipos de datos, incluso con kinds elaborados, son
  promocionados, por ejemplo:

~~~haskell
data Proxy a = Proxy
data App f a = MkApp (f a)   -- App :: forall k. (k -> *) -> k -> *
x = Proxy :: Proxy ('MkApp ('Just 'True))
~~~
 
## Vista general de `TypeInType`

GHC 8 declara que los tipos y los kinds son una sola cosa.

Nada dentro de GHC distingue entre esas dos cosas.

Por ejemplo el tipo `Bool` y el "kind promovido" `Bool` son iguales.

A pesar de eso, el término `True` y el tipo `'True` siguen
distintos, porque el primero puede ser usado en expresiones
y el segundo en tipos.

Esta falta de distincción entre tipos y kinds es característico
de los lenguajes con *tipos dependientes*.

Los lenguajes con tipos *completamente* dependientes también
levantan la diferencia entre expresiones y tipos, pero hacerlo
en GHC no va a ocurrir pronto.

## ¿Porqué no tipos totalmente dependientes?

La idea de `TypeInType` es colapsar tipos y kinds juntos.

Una alternativa es tener una jerarquía infinita:
`Tipos1 :: Tipos2 :: Tipos3 :: ...` (como en Coq).

Una pregunta sería ¿porqué no colapsar también tipos/kinds y valores,
y así tener un lenguaje con tipos dependiente de verdad?

Mantener valores separados permite "erasure semantics": todo lo que
está al nivel de las expresiones es requerido para la ejecución,
y todo lo que está al nivel tipo/kind puede ser borrado para la
ejecución porqué no tiene sentido al nivel calculatorio.

Todavia es una area de investigación muy activa el "erasure analisis"
para lenguajes que tienen un colapso completo entre niveles de
expresión y de tipos.

## Para explorar

* Hoja de ruta para tipos dependientes en GHC 8.2, 8.4 y 8.6
  <https://typesandkinds.wordpress.com/2016/07/24/dependent-types-in-haskell-progress-report/>

* Estatus actual de los tipos dependientes en GHC
  <https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell>

* Type level programming in Haskell (series de 4 posts del 2010):
  <https://byorgey.wordpress.com/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies/>

* Comparación con el lenguaje Agda (en la sección Dependent Types)
  <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>

* An introduction to DataKinds and GHC.TypeLits
  <http://ponies.io/posts/2014-07-30-typelits.html>

* Dependent Types in Haskell - School of Haskell
  <https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell>
