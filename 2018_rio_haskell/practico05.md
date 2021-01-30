% Práctico Haskell 5: DataKinds, Type Families

## Motivación del ejercicio

Todos los programas bien tipados no se ejecutan siempre
bien:

~~~haskell
bottom :: Int
bottom = head [] -- accediendo a la cabeza de una lista vacía!

main :: IO ()
main = print bottom
~~~

¿Podríamos hacer verificar condiciones de limites de listas de forma
estática por el sistema de tipos?

La solución común parametrizar el tamaño de las listas en su tipo. Por ejemplo:

~~~haskell
[] :: Vector Int 0
[1, 2, 3] :: Vector Int 3
~~~

Y así. Se suelen llamar "vectores" las listas parametrizadas por
su tamaño, por eso el nombre del tipo. Si reescribimos el tipo de
`head` en `head :: Vector a (n+1) -> a`, `head []` sería
rechazado en el momento de la compilación porque `[]` tiene el tipo
`Vector a 0` pero el tipo de `head` requiere un tamaño mayor a 0.

El tipo de `Vector` parece *depender* del valor 0, n+1, etc., y tipos
cono este se llaman *tipos dependientes*. Lenguajes como Agda e Idris
proveen tipos completamente dependientes por defecto. En cambio,
Haskell no los provee porque hay inconvenientes para adoptarlos,
como la indecibilidad de la inferencia de tipos, y la pérdida
de la separación entre valores y tipos que impediría tener programas
performantes.

¿Deberíamos renunciar a los tipos dependientes en Haskell? No. Con
las extensiones del sistema de tipo de GHC podemos, en cierta medida,
*simular* tipos dependientes.

¿Como implementarlos? Con respecto a los vectores, debemos considerar
los problemas siguientes:

1. ¿Cómo expresar los enteros naturales al nivel de los tipos?
2. ¿Cómo controlamos el parámetro de tamaño en los tipos?

## Naturales al nivel de los tipos

Veamos como resolver el primer problema. Acá adoptamos los
[números de Peano](www.haskell.org/haskellwiki/Peano_numbers) para
representar enteros naturales:

~~~haskell
data Nat = Z | S Nat
~~~

Es decir, `Z` correponde a 0 y `S n` corresponde a n+1. Por ejemplo
`S ( S (S Z))` significa 3, `S ( S ( S ( S ( S Z))))` significa 5, etc.

Con esto hemos definido enteros naturales al nivel de los *valores*.
Pero necesitamos de hecho enteros naturales al nivel de los *tipos*.
Entonces tenemos que *promover* este *valor* al nivel de los *tipos*.

Afortunadamente, la extensión de GHC `DataKinds` promueve
automáticamente el valor al nivel de los tipos. Consideremos
el código siguiente:

~~~haskell
{-# LANGUAGE DataKinds #-}
data Nat = Z | S Nat
~~~

Con esto, GHC define automáticamente los *constructores de tipos*
`Z` y `S` con sus *kinds* respectivos `Nat` y `Nat -> Nat`. La relación
entre kinds y tipos es la misma que entre tipos y valores, "kind"
significa "tipo del tipo". Los tipos a los cuales estamos acostumbrados,
que tienen algun valor, tienen el kind `*`, por ejemplo, `Int :: *`,
`() :: *` y `Bool :: *`. Los tipos paramétricos (o contenedores)
pueden ser expresados con flechas, como los tipos de funciones.
Por ejemplo:
`[] :: * -> *`, `Either :: * -> * -> *` y
`StateT :: * -> (* -> *) -> * -> *`, etc.

Por defecto, Haskell solo tiene kinds construidos recursivamente a partir
de `*` y `->`.

(Para decir toda la verdad, GHC tiene otro kind básico
`#` que reprensenta tipos "unboxed", pero no lo mencionamos por
simplicidad.  Además existe el kind `Constraint` que sirve para etiquetar
lo que se puede colocar en el contexto de algun tipo. Por ejemplo, el kind
de la clase de tipo `Num` es `* -> Constraint`. El kind del operador de
igualdad de tipos `~` es `k -> k -> Constraint`.)

Luego, la extensión `DataKinds` agrega nuevos kinds
básicos. 

Cabe observar que solo los tipos cuyo kind es `*` pueden tener
habitantes (o, valores), entonces el tipo introducido por `DataKinds`
(es decir `Z` o `S n` como tipo) no puede ser habitado. En particular,
en el protótipo de una función, tales tipos solo pueden ser argumentos
de otros tipos, pero no pueden ocurrir por su cuenta, solos
(¿qué valores tiene el tipo `S n`?).

Entonces, conseguimos números naturales al nivel de los tipos.
Luego, queremos definir alguna función aritmética. Entonces
debemos definir una *función al nivel de los tipos*. La extensión
`TypeFamilies` nos da exactamente lo que queremos. Por ejemplo,
la suma se puede implementar como sigue:

~~~haskell
{-# LANGUAGE DataKinds, TypeFamilies #-}
data Nat = Z | S Nat
type family   Plus (n :: Nat) (m :: Nat) :: Nat
type instance Plus Z     m = m
type instance Plus (S n) m = S (Plus n m)
~~~

Con la extensión `TypeOperators`, hasta podemos escribirla
como un operador infijo:

~~~haskell
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
data Nat = Z | S Nat

infixl 6 :+

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)
~~~

## Ejercicio 1

Implementá el producto `:*` para enteros naturales al nivel de
los tipos. Vas a necesitar la extensión `UndecidableInstances` para
que sea aceptado por GHC.

Hay distintas implementaciones posibles en función de sobre cuál
argumento hacer la recursión y el orden de la recursión y de la suma.

## GADTs

Hemos definido naturales al nivel de los tipos y su aritmética.
¡Implementemos vectores! La extensión *GADTs* (Generalized
Algebraic Data Types) nos permite definir tal tipo de datos:

~~~haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
data Nat = Z | S Nat

infixl 6 :+
-- infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

-- implementación del producto :*

data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-
~~~

Los GADTs nos permiten *especificar* la forma del parámetro de tipo,
especificando explícitamente el tipo del constructor.

Hemos definido el tipo `Vector`, ahora derivemos instancias estándares.
Necesitamos `StandaloneDeriving` porque ``Vector` es un GADT.

~~~haskell
{-# LANGUAGE StandaloneDeriving #-}
deriving instance Eq a => Eq (Vector a n)
deriving instance Show a => Show (Vector a n)
~~~

Ahora que henos definido el tipo `Vector`, implementemos
operaciones sobre vectores. Para la primera, consideremos las funciones
`head` y `tail`. Son bastante directas:

~~~haskell
head :: Vector a (S n) -> a
head (x :- _) = x

tail :: Vector a (S n) -> Vector a n
tail (_ :- xs) = xs

main :: IO ()
main = do
  print $ head (1 :- 2 :- Nil)
  print $ tail (1 :- 2 :- Nil)
  -- | decomentar la línea abajo genera un error de tipeo
  -- print $ head Nil
~~~

Esto nos permite implementar `append` fácilmente:

~~~haskell
append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append (x :- xs) ys = x :- append xs ys
append Nil       ys = ys
~~~

Porque nuestra definición de la suma de los naturales tiene la
recursión en su argumento de la izquierda, y `append` del mismo
lado, GHC puede chequear el tipo de la función con éxito.
¡Hurra!

## Ejercicio 2

 1. Implementá `toList`.
 2. Implementá la versión `Vector` de `map`, `uncons`, `init` y `last`.
 3. Implementá la función siguiente:

    ~~~haskell
    zipWithSame :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
    ~~~

    Es la versión de `zipWith` para vectores de mismo tamaño.

 4.  Implementá la función `min` para enteros naturales al nivel
     de los tipos. Usala para implementar `zipWith` que toma dos
     vectores con tamaños posiblemente distintos.

## Fuente

* [Dependent Types in Haskell de Hiromi ISHII](https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell)
