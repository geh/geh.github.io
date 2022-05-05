% Programación Declarativa - UBP 2020 - Parcial 1


¡Bienvenido/a al primer parcial de Programación Declarativa!

Este parcial tiene una duración de 2hs30.

Para regularizar este parcial, es necesario que:

* hagas la parte "cálculo lambda" de manera manuscripta
* tengas 2 puntos o más en la parte 1, "cálculo lambda"
* tengas 6 puntos o más en la parte 3

El total de los puntos es: 6+8+18+18= 50 puntos.

La nota final sobre 10 será proporcional al total de puntos, y se
aprueba con una nota final de 4 (o sea un total de 20 puntos sobre 50).

Cuando termines (o cuando se termine el tiempo), mandá tu resolución
a la dirección <guillaume.hoffmann@conicet.gov.ar>, con todos los
ejercicios Haskell en un sólo archivo con extensión `.hs`.

Este examen es con libro abierto, tenés acceso a tus apuntes,
podés buscar por internet, pero no podés comunicarte con nadie más
que el profesor hasta que termines. ¡Ánimo!

# Cálculo Lambda: beta reducción (6 puntos)

Considerar los siguientes términos. β-reducirlos hasta obtener su forma normal.

* `(λx.λy.xy)(λy.zyz)`
* `(λx.(λy.x)yλz.z)(λy.yz)(λx.xx)`
* `(λx.λy.y)((λz.zz)(λz.zz))`

# Haskell: Definiciones con guardas o patrones (8 puntos)

Se presentan ejercicios con definiciones no recursivas
de funciones que usan condicionales, guardas o patrones. 

No está permitido usar la estructura `if-then-else`.

~~~haskell
-- ---------------------------------------------------------------------
-- Definir la función 
--    numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,  
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y = undefined

-- ----------------------------------------------------------------------------
-- Los números complejos se pueden representar mediante pares de 
-- números reales. Por ejemplo, el número 2+5i se puede representar mediante 
-- el par (2,5).
-- ----------------------------------------------------------------------------

type Complejo = (Double,Double)

-- ----------------------------------------------------------------------------
-- Definir la función
--    sumaComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(sumaComplejos x y)' es la suma de los números complejos 'x' e 'y'.
-- Por ejemplo,
--    sumaComplejos (2,3) (5,6)  ==  (7.0,9.0)
-- ----------------------------------------------------------------------------

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Definir la función
--    productoComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(productoComplejos x y)' es el producto de los números complejos
-- 'x' e 'y'. Por ejemplo,
--    productoComplejos (2,3) (5,6)  ==  (-8.0,27.0)
-- ----------------------------------------------------------------------------

productoComplejos :: Complejo -> Complejo -> Complejo
productoComplejos (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Definir la función
--    cocienteComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(cocienteComplejos x y)' es el cociente de los números complejos
-- 'x' e 'y'. Por ejemplo,
--    cocienteComplejos (3,2) (1,-2)  ==  (-0.2,1.6)
-- ----------------------------------------------------------------------------

cocienteComplejos :: Complejo -> Complejo -> Complejo
cocienteComplejos (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Definir la función
--    conjugado :: Complejo -> Complejo
-- tal que '(conjugado x)' es el conjugado del número complejo 'x'. Por
-- ejemplo,
--    conjugado (2,3)  ==  (2.0,-3.0)
-- ----------------------------------------------------------------------------

conjugado :: Complejo -> Complejo
conjugado (x1,x2) = undefined

-- ----------------------------------------------------------------------------
-- Los rectángulos pueden representarse por sus dimensiones, base
-- y altura, como un par de números enteros. Por ejemplo, (5,3) representa un
-- rectángulo de base 5 y altura 3.
--
-- Definir la función
--    mayorRectangulo :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que '(mayorRectangulo r1 r2)' es el rectángulo de mayor área entre 'r1'
-- y 'r2'. Por ejemplo,
--    mayorRectangulo (4,6) (3,7)  ==  (4,6)
--    mayorRectangulo (4,6) (3,8)  ==  (4,6)
--    mayorRectangulo (4,6) (3,9)  ==  (3,9)
-- ----------------------------------------------------------------------------

mayorRectangulo :: (Int,Int) -> (Int,Int) -> (Int,Int)
mayorRectangulo (x1,y1) (x2,y2) = undefined

-- ----------------------------------------------------------------------------
-- La disyunción excluyente xor de dos fórmulas se
-- verifica si una es verdadera y la otra es falsa. Su tabla de verdad
-- es
--    x     | y     | xor x y
--    ------+-------+---------
--    True  | True  | False 
--    True  | False | True
--    False | True  | True
--    False | False | False
--    
-- Definir la función 
--    xor1 :: Bool -> Bool -> Bool
-- tal que (xor1 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad. Usar 4 ecuaciones, una por cada línea
-- de la tabla. 
-- ---------------------------------------------------------------------

xor1 :: Bool -> Bool -> Bool
xor1 = undefined

-- ---------------------------------------------------------------------
-- Definir la función 
--    xor2 :: Bool -> Bool -> Bool
-- tal que (xor2 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por
-- cada valor del primer argumento. 
-- ---------------------------------------------------------------------

xor2 :: Bool -> Bool -> Bool
xor2 = undefined
~~~

# Haskell: funciones sobre tipos de datos algebraicos (18 puntos)

## Comida vegatariana (8 puntos)

Considerá los tipos y constantes siguientes:

~~~haskell
data Comida = Pizza [Ingrediente] | Empanada [Ingrediente]
  deriving Show

data Ingrediente = Tomate   -- ingredientes vegetarianos
                 | Queso    --
                 | Morron   --
                 | Jamon         -- ingredientes no vegetarianos
                 | CarneMolida   --
  deriving Show

type Pedido = [Comida]

muzzarella, especial, caprese, criolla :: Comida
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

-- Para ser considerado "carnivoro", un pedido tiene que
-- ser tal que todas las comidas pedidas sean no-vegetarianas:
--   esPedidoCarnivoro [especial,jyc,jyc] == True
--   esPedidoCarnivoro [especial,jyc,caprese] == Fale

esPedidoCarnivoro :: Pedido -> Bool
esPedidoCarnivoro = undefined
~~~

## El tipo `Maybe` (10 puntos)

Tomando en cuenta que el tipo siguiente está definido en Haskell:

~~~haskell
data Maybe a = Nothing | Just a
~~~

Definí las funciones siguientes, sin usar `head` u otras funciones
imcompletas:

~~~haskell
-- `divisionSegura` toma dos enteros y devuelve Nothing si el segundo es cero,
-- o Just c donde c es el cociente del primero por el segundo.
divisionSegura :: Int -> Int -> Maybe Int
divisionSegura = undefined

-- `sumaMaybe` devuelve un Just de la suma si los dos parametros
-- son Just. En los otros casos, devuelve Nothing.
sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybe = undefined

-- cabeza [1,2,3,4,5] == Just 1
-- cabeza [] == Nothing
cabeza :: [a] -> Maybe a
cabeza = undefined

-- ultimo [1,2,3,4,5] == Just 5
-- ultimo [] == Nothing
ultimo :: [a] -> Maybe a
ultimo = undefined

-- limpiar [Nothing, Just 10, Just 5] == [10,5] 
limpiar :: [Maybe a] -> [a]
limpiar = undefined
~~~

# Haskell: recursion, uso de funciones de orden superior, y foldr/foldl (18 puntos)

~~~haskell
-- ---------------------------------------------------------------------
-- Defini por recursion la funcion
--    replicate' :: Int -> a -> [a]
-- tal que (replicate' n x) es la lista formado por n copias del
-- elemento x. Por ejemplo,
--    replicate' 3 2  ==  [2,2,2]
-- ---------------------------------------------------------------------
 
replicate' :: Int -> a -> [a]
replicate' = undefined

-- ---------------------------------------------------------------------
-- Defini por recursion la funcion
--    elem' :: Eq a => a -> [a] -> Bool
-- tal que (elem' x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo, 
--    elem' 3 [2,3,5]  ==  True
--    elem' 4 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

elem' :: Eq a => a -> [a] -> Bool
elem' = undefined

-- ---------------------------------------------------------------------
-- Defini por recursion la función interseccion que dadas dos listas devuelve la
-- lista de los elementos que están en las dos. 
--
--   interseccion [4,2,6,1,4] [1,2,5] == [2,1] 
-- 
-- Podés usar la función elem para definir interseccion
-- ---------------------------------------------------------------------

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion = undefined

-- ----------------------------------------------------------------------------
-- Considera la funcion
--    suma :: Num b => (a -> b) -> [a] -> b
-- tal que '(suma f xs)' es la suma de los valores obtenidos aplicando la
-- funcion 'f' a los elementos de la lista 'xs'. Por ejemplo,
--    suma (*2)  [3,5,10]  ==  36
--    suma (/10) [3,5,10]  ==  1.8
--
-- Defini esta funcion
-- 1) usando funciones de orden superior
-- 2) por recursion
-- 3) por plegado (con foldr)
-- ----------------------------------------------------------------------------

-- 1) La definicion usando funciones de orden superior es
sumaS :: Num b => (a -> b) -> [a] -> b
sumaS = undefined

-- 2) La definicion por recursion es
sumaR :: Num b => (a -> b) -> [a] -> b
sumaR = undefined

-- 3) La definicion por plegado (foldr) es
sumaP :: Num b => (a -> b) -> [a] -> b
sumaP = undefined

-- ----------------------------------------------------------------------------
-- Considera la funcion
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que '(productoPred p xs)' es el producto de los elementos de la lista
-- 'xs' que verifican el predicado 'p'. Por ejemplo,
--    productoPred even [2,1,-3,4,-5,6]  ==  48
--
-- Defini esta funcion
-- 1) usando funciones de orden superior
-- 2) por recursion
-- 3) por plegado (con foldr)
-- ----------------------------------------------------------------------------

-- 1) La definicion usando funciones de orden superior es
productoPredS :: Num a => (a -> Bool) -> [a] -> a
productoPredS = undefined

-- 2) La definicion por recursion es
productoPredR :: Num a => (a -> Bool) -> [a] -> a
productoPredR = undefined

-- 3) La definicion por plegado (foldr) es
productoPredP :: Num a => (a -> Bool) -> [a] -> a
productoPredP = undefined

-- ---------------------------------------------------------------------
-- Redefini por recursión la función
--    takeWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhile p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhile' (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------
 
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = undefined
 
-- ---------------------------------------------------------------------
-- Redefini por recursión la función
--    dropWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhile p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que cumple la propiedad p. Por ejemplo,
--    dropWhile' (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------
 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = undefined
 
-- ---------------------------------------------------------------------
-- Redefini, usando foldr, la función concat. Por ejemplo, 
--    concat' [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------
 
concat' :: [[a]] -> [a]
concat' = undefined
 
-- ---------------------------------------------------------------------
-- Considera la función 
--    filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplica (4+) (<3) [1..7]  =>  [5,6]
-- Defini la función
-- 1. usando map y filter,
-- 2. por recursión y
-- 3. por plegado (con foldr).
-- ---------------------------------------------------------------------
 
-- La definición con map y filter es
filtraAplica_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_1 f p xs = undefined
 
-- La definición por recursión es
filtraAplica_2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_2 f p [] = undefined
 
-- La definición por plegado es
filtraAplica_3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_3 f p = undefined
 
-- ---------------------------------------------------------------------
-- Considera la función
--    maximum :: Ord a => [a] -> a
-- tal que (maximum xs) es el máximo de la lista xs. Por ejemplo,
--    maximum [3,7,2,5]                  ==  7
--    maximum ["todo","es","falso"]      ==  "todo"
--    maximum ["menos","alguna","cosa"]  ==  "menos"
-- Defini la función
-- 1. por recursión y
-- 2. por plegado (con foldr1 :: (a -> a -> a) -> [a] -> a).
-- ---------------------------------------------------------------------
 
-- 1) La definicion por recursión es
maximumR :: Ord a => [a] -> a
maximumR = undefined
 
-- 2) La definicion por plegado (foldr1) es
maximumP :: Ord a => [a] -> a
maximumP = undefined
 
-- ---------------------------------------------------------------------
-- Defini, mediante plegado con foldr1, la función
--    minimumP :: Ord a => [a] -> a
-- tal que (minimunR xs) es el máximo de la lista xs. Por ejemplo,
--    minimumP [3,7,2,5]                  ==  2
--    minimumP ["todo","es","falso"]      ==  "es"
--    minimumP ["menos","alguna","cosa"]  ==  "alguna"
-- Nota: La función minimunP es equivalente a la predefinida minimun.
-- ---------------------------------------------------------------------
 
minimumP :: Ord a => [a] -> a
minimumP = undefined
~~~



