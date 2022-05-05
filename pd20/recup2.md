% Prog. Declarativa: Recuperatorio 2

El tiempo para resolver el recuperatorio es 1h15.

Contestá todos los ejercicios en un mismo módulo,
cuyo nombre es `ApellidoNombre.hs`, donde
`Apellido` es tu primer apellido y `Nombre` es tu
primer nombre.

Mandá este archivo por mail como archivo adjunto sin
compresión a <guillaume.hoffmann@conicet.gov.ar>,
y el asunto del mail siendo `Parcial 2 Declarativa`.

# Encabezado del módulo

Pegá las líneas siguientes en el módulo:

~~~haskell
module ApellidoNombre where -- pone ahí el nombre del módulo

import Data.List (intercalate)
import Data.Map.Strict

-- tipo de lista asociativa
data ListAssoc a b = Empty
                   | Node a b (ListAssoc a b)
        deriving Show

-- A continuación, la resolución de los ejercicios:

instance (Show a, Show b) => Show (ListAssoc a b) where
     show = undefined

~~~

# 1. Instancias de `Show`,`Semigroup` y `Monoid` (5 puntos)

1. Implementá la instancia de esta lista para la clase `Show`,
   de tal manera que la lista se imprima como si fuera una lista de
   tuplas:

~~~haskell
show Vac == "[]"
show (Node 1 2 (Node 2 3 Vac)) == "[(1,2),(2,3)]"
~~~

   Para insertar las comas, es recomendable usar la función `intercalate`.

 2. `comodin :: Int -> ListAssoc Int Int` que, dada un entero positivo `n`,
    construye un lista asociativa de `n` entradas desde `n` hasta `1`, cada
    entrada teniendo asociado el valor 0. Por ejemplo:

    ~~~haskell
    comodin 1 == Node 1 0 Empty
    comodin 3 == Node 3 0 (Node 2 0 (Node 1 0 Empty))
    comodin 0 == Empty
    ~~~

 3. Implementá `existe :: Eq a => ListAssoc a b -> a -> Bool` que dada una lista y una
    clave devuelve `True` si la clave está y `False` en caso contrario.
 4. Implementá `agregar :: Eq a => a -> b -> ListAssoc a b -> ListAssoc a b` que dada
    una clave `a`, un dato `b` lo agrega en la lista si la clave `a` no existe.
    En caso de que la clave `a` exista, se reemplaza el dato anterior.
 5. Hacé que el tipo `ListAssoc` pertenezca a la clase `Semigroup` de tal forma
    que cuando se combinan dos `ListAssoc` con `<>` , y existen claves duplicadas, se descarte
    el dato correspondiente de la `ListAssoc` de la izquierda y quede el de la derecha.
    Implementá también la instancia de `ListAssoc` para `Monoid`.

# 2. El tipo diccionario (Map) y funciones IO (2 puntos)

1. Implementá la función `laToDisk :: (Show a, Show b) => Listassoc a b -> FilePath -> IO ()` que convierte una `ListAssoc` a
   una cadena de caracteres, y la guarda en el camino indicado por el parámetro `FilePath`.

2. Implementá la función `laToMap :: (Ord a) => ListAssoc a b -> Map a b` que convierte la lista a un diccionario.
   Podés suponer que no hay claves duplicadas en la lista asociativa.

# 3. QuickCheck (3 puntos)

Agregá lo siguiente a tu módulo:

~~~haskell
import Test.QuickCheck
~~~

1. Implementá `laLength :: ListAssoc a b -> Int`, el equivalente de `length` para un `Lef`.

2. Implementa una propiedad que indica que la suma de los tamaños de dos `ListAssoc`, `l1` y `l2`
   es igual al tamaño de `l1 <> l2`.

3. Implementa una instancia de `Lef` para `Arbitrary`. Hacélo con unos de los métodos indicados
   en el [teórico sobre QuickCheck](https://cs.famaf.unc.edu.ar/~hoffmann/pd20/quickcheck.html).
   Comprobá la propiedad anterior en `ghci`.


