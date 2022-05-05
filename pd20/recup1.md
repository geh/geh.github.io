% Programación Declarativa - UBP 2020 - Recuperatorio 1

Este recuperatorio tiene una duración de 1h15.

Contestá todos los ejercicios en un mismo módulo,
cuyo nombre es `ApellidoNombre.hs`, donde
`Apellido` es tu primer apellido y `Nombre` es tu
primer nombre.

Mandá este archivo por mail como archivo adjunto sin
compresión a <guillaume.hoffmann@conicet.gov.ar>, y el asunto del mail
siendo `Recuperatorio 1 Declarativa`.

Este examen es con libro abierto, tenés acceso a tus apuntes,
podés buscar por internet, pero no podés comunicarte con nadie más
que el profesor hasta que termines. ¡Ánimo!

# Funciones sobre tipos de datos algebraicos (5 puntos)

## Comida vegatariana (2 puntos)

Considerá los tipos y constantes siguientes:

~~~haskell
data Comida = Empanada [Ingrediente] | Pizza [Ingrediente]
  deriving Show

data Ingrediente = Tomate   -- ingredientes vegetarianos
                 | Queso    --
                 | Morron   --
                 | Jamon         -- ingredientes no vegetarianos
                 | CarneMolida   --
  deriving Show

type Pedido = [Comida]

muzzarella, especial, caprese, criolla, jyc :: Comida
muzzarella = Pizza [Tomate,Queso]
especial   = Pizza [Tomate,Queso,Jamon,Morron]
criolla    = Empanada [CarneMolida]
caprese    = Empanada [Tomate,Queso]
jyc        = Empanada [Jamon,Queso]
~~~

Definí las funciones siguientes, que determinan si un
ingrediente, una comida o un pedido son vegetarianos.
Tomá en cuenta los comentarios más arriba.

~~~haskell
esVeg :: Ingrediente -> Bool
esVeg = undefined

esComidaVeg :: Comida -> Bool
esComidaVeg = undefined

-- Para ser vegetariano, un pedido
-- tiene que ser 100% vegetariano
--     esPedidoVeg [muzzarella, caprese] == True
--     esPedidoVeg [muzzarella, caprese, especial] == False
esPedidoVeg :: Pedido -> Bool
esPedidoVeg = undefined
~~~

## El tipo `Either` (2 puntos)

Tomando en cuenta que el tipo siguiente está definido en Haskell:

~~~haskell
data Either a b = Left a | Right b
~~~

Definí las funciones siguientes, sin usar `head` u otras funciones imcompletas:

~~~haskell
-- `divisionSegura` toma dos enteros , y devuelve `Left "Division por cero"` si el segundo es cero,
-- sino `Right c` donde `c` es el cociente de los dos parametros.

divisionSegura :: Int -> Int -> Either String Int
divisionSegura = undefined

-- `sumaEither` devuelve un `Right` de la suma si los dos parametros
-- son `Right`. En los otros casos, devuelve un `Left` de la suma de los enteros provistos.
sumaEither :: Either Int Int -> Either Int Int -> Either Int Int
sumaEither = undefined

-- cabeza [1,2,3,4,5] == Right 1
-- cabeza [] == Left "Lista vacia"
cabeza :: [a] -> Either String a
cabeza = undefined

-- ultimo [1,2,3,4,5] == Right 5
-- ultimo [] == Left "List vacia"
ultimo :: [a] -> Either String a
ultimo = undefined

-- limpiar [Left 5, Just 10, Just 5, Left 1] == [10,5] 
limpiar :: [Either a a] -> [a]
limpiar = undefined
~~~

# Recursión, uso de funciones de orden superior, y foldr/foldl (6 puntos)

~~~haskell
-- ------------------------------------------------------------------
-- Defini por recursion la funcion
-- unzip' :: [(a,b)] -> ([a],[b])
-- ejemplo:
--    unzip' [(a,1),(b,2),(c,3)] == ("abc",[1,2,3])
-- ------------------------------------------------------------------

unzip' :: [(a,b)] -> ([a],[b])
unzip' = undefinded

-- ------------------------------------------------------------------
-- Defini por recursion la función menorA :: Int -> [Int] -> [Int]
-- que recibe un número n y una lista de números l, y devuelve la
-- sublista de l de los menores a n. 
--    menorA 20 [23,5,16,38,11,24] == [5,16,11]
-- ------------------------------------------------------------------

menorA :: Int -> [Int] -> [Int]
menorA = undefined

-- ------------------------------------------------------------------
-- Definí por recursión la función aplanar que dada una lista de
-- listas, devuelva una lista que concatene a todas ellas. Ejemplo:
--    aplanar [[1], [3, 1, 2], [], [4, 4, 8]] == [1,3,1,2,4,4,8]
-- ------------------------------------------------------------------

aplanar :: [[a]] -> [a]
aplanar = undefined

-- ------------------------------------------------------------------
-- Implementá por recursión la función `rayuela :: [a] -> [[a]]`.
--  Su salida es una lista de listas. La primera lista de su salida
-- es la misma que la lista de entrada. La segunda contiene cada
-- segundo elemento de la lista de entrada.... la `n`ésima contiene
-- cada `n`ésimo elemento de la lista de entrada.
--    rayuela "ABCD"       == ["ABCD", "BD", "C", "D"]
--    rayuela "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
--    rayuela [1]          == [[1]]
--    rayuela [True,False] == [[True,False], [False]]
--    rayuela []           == []
--
-- Fijate que la salida tiene el mismo tamaño que la entrada.
-- ------------------------------------------------------------------

rayuela :: [a] -> [[a]]
rayuela = undefined

-- ------------------------------------------------------------------
-- Considera la funcion
--    suma :: Num b => (a -> b) -> [a] -> b
-- tal que '(suma f xs)' es la suma de los valores obtenidosi
-- aplicando la funcion 'f' a los elementos de la lista 'xs'.
-- Por ejemplo,
--    suma (*2)  [3,5,10]  ==  36
--    suma (/10) [3,5,10]  ==  1.8
--
-- Defini esta funcion
-- 1) usando funciones de orden superior
-- 2) por recursion
-- 3) por plegado (con foldr)
-- -----------------------------------------------------------------

-- 1) La definicion usando funciones de orden superior es
sumaS :: Num b => (a -> b) -> [a] -> b
sumaS = undefined

-- 2) La definicion por recursion es
sumaR :: Num b => (a -> b) -> [a] -> b
sumaR = undefined

-- 3) La definicion por plegado (foldr) es
sumaP :: Num b => (a -> b) -> [a] -> b
sumaP = undefined

-- -----------------------------------------------------------------
-- Considera la funcion
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que '(productoPred p xs)' es el producto de los elementos
-- de la lista 'xs' que verifican el predicado 'p'. Por ejemplo,
--    productoPred even [2,1,-3,4,-5,6]  ==  48
--
-- Defini esta funcion
-- 1) usando funciones de orden superior
-- 2) por recursion
-- 3) por plegado (con foldr)
-- -----------------------------------------------------------------

-- 1) La definicion usando funciones de orden superior es
productoPredS :: Num a => (a -> Bool) -> [a] -> a
productoPredS = undefined

-- 2) La definicion por recursion es
productoPredR :: Num a => (a -> Bool) -> [a] -> a
productoPredR = undefined

-- 3) La definicion por plegado (foldr) es
productoPredP :: Num a => (a -> Bool) -> [a] -> a
productoPredP = undefined
~~~
