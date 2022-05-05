% Prog. Declarativa: Parcial 2
% 18 de noviembre 2020

Contestá todos los ejercicios en un mismo módulo,
cuyo nombre es `ApellidoNombre.hs`, donde
`Apellido` es tu primer apellido y `Nombre` es tu
primer nombre.

Mandá este archivo por mail como archivo adjunto sin
compresión a <guillaume.hoffmann@conicet.gov.ar>, y el asunto del mail
siendo `Parcial 2 Declarativa`.

Si no cumplís con este formato, tu parcial no será
corregido y tendrá una nota de 1.

Debés lograr 2 puntos en la sección 1 para regularizar
el parcial.

El tiempo para resolver el parcial es de 1 hora y 15 minutos.

# Encabezado del módulo

Pegá las líneas siguientes en el módulo:

~~~haskell
module ApellidoNombre where -- pone ahí el nombre del módulo

import Data.List (intercalate)
import Data.Map.Strict

-- Este tipo de datos representa listas
-- donde cada elemento puede ser o un entero o un flotante:

data Lef = Vac | E Int Lef | F Float Lef

-- A continuación, la resolución de los ejercicios:
~~~

# 1. Instancias de `Show`, `Eq`, `Semigroup` y `Monoid` (5 puntos)

Esta sección tiene 5 ejercicios pero solo tendrás que implementar uno
de los dos primeros; leé con atención los enunciados.

1. Si tu primer nombre *y* tu primer apellido empiezan con una consonante, no hagas este
   ejercicio, solo agregá `deriving Show` al tipo de datos `Lef` y pasá al ejercicio siguiente.

   Sino, implementá la instancia de esta lista para la clase `Show`,
   donde se deberá cumplir lo siguiente:

~~~haskell
show Vac == "[]"
show (E 1 (E 2 ( F 3.1 Vac))) == "[1,2,3.1]"
show (F 3.1 (F 3.14 ( F 3.142 Vac))) == "[3.1,3.14,3.142]"
~~~

   Para insertar las comas, es recomendable usar la función `intercalate`.

2. Si tu primer nombre *o* tu primer apellido empieza con una vocal, no hagas este
   ejercicio, solo agregá `deriving Eq` al tipo de datos `Lef` y pasá al ejercicio siguiente.

   Sino, implementá la instancia de esta lista para la clase `Eq`.
   Para ello, deberás tomar en cuenta:
   1. que el tamaño de las listas son iguales
   2. que los elementos de la lista son iguales, con la
      tolerancia siguiente: se considera que un valor flotante `N.0` es igual al entero `N`.

Ejemplos:

~~~haskell
E 1 Vac    == F 1.0 Vac    = True
F 1.0 Vac  == E 1 Vac    = True
E 1 Vac    == F 1.1 Vac    = False
~~~

Para comprobar esta "tolerancia", declará y usá la siguiente función local dentro de
tu definición de  `==`:

~~~haskell
isInt x = x == fromInteger (round x)
~~~

3. Implementá una función `enumE :: Int -> Int -> Lef`
para generar listas de enteros del rango dado
por sus parámetros, con paso de 1:

~~~haskell
enumE x y == E x ( E (x+1) (... E y Vac))
enumE x x == E x Vac
enumE 10 9 == Vac
~~~

Implementá también una función `enumF :: Int -> Int -> Lef`
que genera listas de flotantes (usá `fromIntegral` para convertir un valor `Int` a `Float`).

4. Implementa la instancia de `Lef` para `Semigroup`, donde el operador `<>` consiste
   en concatenar los dos `Lef`.
   Podrás probar en `ghci` tu implementación de `<>` con:
   `enumE 1 3  <> enumF 4 6` y viendo el resultado.

5. Implementá la instancia de `Lef` para `Monoid`, donde el neutro de la operación `<>`
   es la lista vacía.


# 2. El tipo diccionario (Map) y funciones IO (2 puntos)

En el mismo módulo:

1. Implementá una función `lefToDisk :: Lef -> FilePath -> IO ()` que convierte una `Lef` a
   una cadena de caracteres, y la guarda en el camino indicado por el parámetro `FilePath`.

2. Implementá una función `lefToMap :: Lef -> Map Int Int` que se comporta de la manera siguiente:
   * si la `Lef` es vacía, genera el diccionario vacío
   * almacena en el diccionario la cantidad de veces que aparece cada valor entero del `Lef`
   * no toma en cuenta los valores flotantes del `Lef`

# 3. QuickCheck (3 puntos)

Agregá lo siguiente a tu módulo:

~~~haskell
import Test.QuickCheck
~~~

1. Implementá `lefLength :: Lef -> Int`, el equivalente de `length` para un `Lef`.

2. Implementa una propiedad que indica que la suma de los tamaños de dos `Lef`, `l1` y `l2`
   es igual al tamaño de `l1 <> l2`.

3. Implementa una instancia de `Lef` para `Arbritrary`. Hacélo con unos de los métodos indicados
   en el [teórico sobre QuickCheck](https://cs.famaf.unc.edu.ar/~hoffmann/pd20/quickcheck.html).
   Comprobá la propiedad anterior en `ghci`.



