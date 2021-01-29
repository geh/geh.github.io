-- Funciones de orden superior: `map`, `filter`, `foldr` y otras

-- ============================================================================
-- Librerias auxiliares
-- ============================================================================

import Data.Char

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Se considera la funcion
--    sumaDigitos :: String -> Int
-- tal que '(sumaDigitos xs)' es la suma de los digitos de la cadena 'xs'. Por
-- ejemplo,
--    sumaDigitos "SE 2431 X"  ==  10
--
-- Se pide, definir esta funcion
-- 1) usando funciones de orden superior
-- 2) por recursion
-- 3) por plegado (con foldr)
-- Nota: Usar las funciones '(isDigit c)' que se verifica si el caracter 'c' es
-- un digito y '(digitToInt d)' que es el entero correspondiente al digito 'd'.
-- ----------------------------------------------------------------------------

-- 1) La definicion usando funciones de orden superior es
sumaDigitosS :: String -> Int
sumaDigitosS = undefined

-- 2) La definicion por recursion es
sumaDigitosR :: String -> Int
sumaDigitosR = undefined

-- 3) La definicion por plegado (foldr) es
sumaDigitosP :: String -> Int
sumaDigitosP = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Se considera la funcion
--    mayusculaInicial :: String -> String
-- tal que '(mayusculaInicial xs)' es la palabra 'xs' con la letra inicial en
-- mayuscula y las restantes en minusculas. Por ejemplo,
--    mayusculaInicial "sEviLLa"  ==  "Sevilla"
--
-- Se pide, definir esta funcion
-- 1) usando funciones de orden superior
-- 2) por recursion
-- 3) por plegado (con foldr)
-- Nota: Usar las funciones '(toLower c)' que es el caracter 'c' en minuscula y
-- '(toUpper c)' que es el caracter 'c' en mayuscula.
-- ----------------------------------------------------------------------------

-- 1) La definicion usando funciones de orden superior es
mayusculaInicialS :: String -> String
mayusculaInicialS  = undefined

-- 2) La definicion por recursion es
mayusculaInicialR :: String -> String
mayusculaInicialR = undefined

-- 3) La definicion por plegado (foldr) es
mayusculaInicialP :: String -> String
mayusculaInicialP = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Se considera la funcion
--    suma :: Num b => (a -> b) -> [a] -> b
-- tal que '(suma f xs)' es la suma de los valores obtenidos aplicando la
-- funcion 'f' a los elementos de la lista 'xs'. Por ejemplo,
--    suma (*2)  [3,5,10]  ==  36
--    suma (/10) [3,5,10]  ==  1.8
--
-- Se pide, definir esta funcion
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
-- Ejercicio 4. Se considera la funcion
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que '(productoPred p xs)' es el producto de los elementos de la lista
-- 'xs' que verifican el predicado 'p'. Por ejemplo,
--    productoPred even [2,1,-3,4,-5,6]  ==  48
--
-- Se pide, definir esta funcion
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
-- Ejercicio 5. Redefinir por recursión la función
--    takeWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhile p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhile' (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------
 
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Redefinir por recursión la función
--    dropWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhile p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que cumple la propiedad p. Por ejemplo,
--    dropWhile' (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------
 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 7. Redefinir, usando foldr, la función concat. Por ejemplo, 
--    concat' [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------
 
concat' :: [[a]] -> [a]
concat' = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 8. Se considera la función 
--    filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplica (4+) (<3) [1..7]  =>  [5,6]
-- Se pide, definir la función
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
-- Ejercicio 9. Se considera la función
--    maximum :: Ord a => [a] -> a
-- tal que (maximum xs) es el máximo de la lista xs. Por ejemplo,
--    maximum [3,7,2,5]                  ==  7
--    maximum ["todo","es","falso"]      ==  "todo"
--    maximum ["menos","alguna","cosa"]  ==  "menos"
-- Se pide, definir la función
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
-- Ejercicio 10. Definir, mediante plegado con foldr1, la función
--    minimumP :: Ord a => [a] -> a
-- tal que (minimunR xs) es el máximo de la lista xs. Por ejemplo,
--    minimumP [3,7,2,5]                  ==  2
--    minimumP ["todo","es","falso"]      ==  "es"
--    minimumP ["menos","alguna","cosa"]  ==  "alguna"
-- Nota: La función minimunP es equivalente a la predefinida minimun.
-- ---------------------------------------------------------------------
 
minimumP :: Ord a => [a] -> a
minimumP = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Se considera la función
--    resultadoPositivo :: (a -> Integer) -> [a] -> [a]
-- tal que '(resultadoPositivo f xs)' es la lista de los elementos de 'xs'
-- tales que el valor de la función 'f' sobre ellos es positivo. Por ejemplo,
--    resultadoPositivo head [[-1,2],[-9,4],[2,3]]       ==  [[2,3]]
--    resultadoPositivo sum [[1,2],[9],[-8,3],[],[3,5]]  ==  [[1,2],[9],[3,5]]
--
-- Se pide definir esta función
-- 1) por recursión,
-- 2) por plegado (con foldr),
-- 3) por recursión con acumulador,
-- 4) por plegado (con foldl)
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
resultadoPositivoR :: (a -> Integer) -> [a] -> [a]
resultadoPositivoR = undefined

-- 2) La definición por plegado (con foldr) es
resultadoPositivoP :: (a -> Integer) -> [a] -> [a]
resultadoPositivoP = undefined

-- 3) La definición por recursión con acumulador es
resultadoPositivoAR :: (a -> Integer) -> [a] -> [a]
resultadoPositivoAR = undefined

-- 4) La definición por plegado (con foldl) es
resultadoPositivoAP :: (a -> Integer) -> [a] -> [a]
resultadoPositivoAP = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Se considera la función
--    intercala x ys :: Integer -> [Integer] -> [Integer]
-- tal que '(intercala x ys)' es la lista que resulta de intercalar el elemento
-- 'x' delante de todos los elementos de 'ys' que sean menores que 'x'. Por
-- ejemplo,
--    intercala 5 [1,2,6,3,7,9]  ==  [5,1,5,2,6,5,3,7,9]
--    intercala 5 [6,7,9,8]      ==  [6,7,9,8]
--
-- Se pide definir esta función
-- 1) por recursión,
-- 2) por plegado (con foldr),
-- 3) por recursión con acumulador,
-- 4) por plegado (con foldl)
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
intercalaR :: Integer -> [Integer] -> [Integer]
intercalaR = undefined

-- 2) La definición por plegado (con foldr) es
intercalaP :: Integer -> [Integer] -> [Integer]
intercalaP  = undefined

-- 3) La definición por recursión con acumulador es
intercalaAR :: Integer -> [Integer] -> [Integer]
intercalaAR  = undefined

-- 4) La definición por plegado (con foldl) es
intercalaAP :: Integer -> [Integer] -> [Integer]
intercalaAP = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Se considera la función
--    dec2ent :: [Int] -> Int
-- tal que '(dec2ent xs)' es el número entero correspondiente a la expresión
-- decimal 'xs'. Por ejemplo,
--    dec2ent [2,3,4,5]  ==  2345
--    dec2ent [1..9]     ==  123456789
--
-- Se pide definir esta función
-- 1) por recursión con acumulador,
-- 2) por plegado (con foldl)
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
dec2entAR :: [Int] -> Int
dec2entAR = undefined

-- 2) La definición por plegado (con foldr) es
dec2entAP :: [Int] -> Int
dec2entAP = undefined

-- 3) La definición por plegado (con foldl) es
dec2entAPL :: [Int] -> Int
dec2entAPL  = undefined

-- ====================================================================

-- Fuente:
-- Departamento de Ciencias de la Computacion e Inteligencia Artificial
-- Universidad de Sevilla
