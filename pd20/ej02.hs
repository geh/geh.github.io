-- Definiciones con condicionales, guardas o patrones.
-- =====================================================================
-- ---------------------------------------------------------------------
-- Introduccion                                                       --
-- ---------------------------------------------------------------------

-- En esta relacion se presentan ejercicios con definiciones elementales
-- (no recursivas) de funciones que usan condicionales, guardas o
-- patrones. 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
--    divisionSegura 7 2  ==  3.5
--    divisionSegura 7 0  ==  9999.0
-- Nota: hacer diferentes definiciones, con ecuaciones, condicionales y
-- con guardas.  Recuerda poner nombres distintos para cada definicion.
-- ---------------------------------------------------------------------

divisionSegura :: Double -> Double -> Double
divisionSegura = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p)  es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo, 
--    intercambia (2,5)  ==  (5,2)
--    intercambia (5,2)  ==  (2,5)
-- ---------------------------------------------------------------------

intercambia :: (a,b) -> (b,a)
intercambia = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir una función 
--    ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los
-- elementos de la lista xs, pasando el último elemento al principio de
-- la lista. Por ejemplo, 
--    ciclo [2,5,7,9]  == [9,2,5,7]
--    ciclo []         == []
--    ciclo [2]        == [2]
-- ---------------------------------------------------------------------

ciclo :: [a] -> [a]
ciclo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,  
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir el operador
--    (~=) :: (Fractional a, Ord a) => a -> a -> Bool
-- tal que (x ~= y) se verifica si x e y son casi iguales; es decir si
-- el valor absoluto de su diferencia es menor que una milésima. Por
-- ejemplo, 
--    12.3457 ~= 12.3459  ==  True
--    12.3457 ~= 12.3479  ==  False
-- ---------------------------------------------------------------------

(~=) :: (Fractional a, Ord a) => a -> a -> Bool
x ~= y = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. En geometría, la fórmula de Herón, descubierta por
-- Herón de Alejandría, dice que el área de un triángulo cuyo lados
-- miden a, b y c es la raíz cuadrada de s(s-a)(s-b)(s-c) donde s es el
-- semiperímetro 
--    s = (a+b+c)/2
-- 
-- Definir la función 
--    area :: Double -> Double -> Double -> Double 
-- tal que (area a b c) es el área del triángulo de lados a, b y c. Por
-- ejemplo, 
--    area 3 4 5  ==  6.0
-- ---------------------------------------------------------------------

area :: Double -> Double -> Double -> Double 
area a b c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Los intervalos cerrados se pueden representar mediante
-- una lista de dos números (el primero es el extremo inferior del
-- intervalo y el segundo el superior). 
-- 
-- Definir la función 
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e
-- i2. Por ejemplo,
--    interseccion [] [3,5]     ==  []
--    interseccion [3,5] []     ==  []
--    interseccion [2,4] [6,9]  ==  []
--    interseccion [2,6] [6,9]  ==  [6,6]
--    interseccion [2,6] [0,9]  ==  [2,6]
--    interseccion [2,6] [0,4]  ==  [2,4]
--    interseccion [4,6] [0,4]  ==  [4,4]
--    interseccion [5,6] [0,4]  ==  []
-- ---------------------------------------------------------------------

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Los números racionales pueden representarse mediante
-- pares de números enteros. Por ejemplo, el número 2/5 puede
-- representarse mediante el par (2,5). 
-- 
-- Definir la función 
--    formaReducida :: (Int,Int) -> (Int,Int) 
-- tal que (formaReducida x) es la forma reducida del número racional
-- x. Por ejemplo, 
--    formaReducida (4,10)  ==  (2,5)
-- Nota: usar la función gcd
-- ---------------------------------------------------------------------

formaReducida :: (Int,Int) -> (Int,Int) 
formaReducida (a,b) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función 
--    sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (sumaRacional x y) es la suma de los números racionales x e
-- y, expresada en forma reducida. Por ejemplo, 
--    sumaRacional (2,3) (5,6)  ==  (3,2)
-- ---------------------------------------------------------------------

sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional (a,b) (c,d) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función 
--    productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (productoRacional x y) es el producto de los números
-- racionales x e y. Por ejemplo, 
--    productoRacional (2,3) (5,6)  ==  (5,9)
-- ---------------------------------------------------------------------

productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
productoRacional (a,b) (c,d) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    cocienteRacional ::  (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que '(cocienteRacional x y)' es el cociente de los números racionales
-- 'x' e 'y'. Por ejemplo,
--    cocienteRacional (2,3) (5,6)  ==  (4,5)
-- ----------------------------------------------------------------------------

cocienteRacional ::  (Int,Int) -> (Int,Int) -> (Int,Int)
cocienteRacional (x1,x2) (y1,y2) = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función 
--    igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
-- tal que (igualdadRacional x y) se verifica si los números racionales
-- x e y son iguales. Por ejemplo, 
--    igualdadRacional (6,9) (10,15)  ==  True
--    igualdadRacional (6,9) (11,15)  ==  False
--    igualdadRacional (0,2) (0,-5)   ==  True
-- ---------------------------------------------------------------------

igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional (a,b) (c,d) = undefined

-- ----------------------------------------------------------------------------

-- Ejercicio 14. Los números complejos se pueden representar mediante pares de 
-- números reales. Por ejemplo, el número 2+5i se puede representar mediante 
-- el par (2,5).
-- ----------------------------------------------------------------------------

type Complejo = (Double,Double)

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    sumaComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(sumaComplejos x y)' es la suma de los números complejos 'x' e 'y'.
-- Por ejemplo,
--    sumaComplejos (2,3) (5,6)  ==  (7.0,9.0)
-- ----------------------------------------------------------------------------

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    productoComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(productoComplejos x y)' es el producto de los números complejos
-- 'x' e 'y'. Por ejemplo,
--    productoComplejos (2,3) (5,6)  ==  (-8.0,27.0)
-- ----------------------------------------------------------------------------

productoComplejos :: Complejo -> Complejo -> Complejo
productoComplejos (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    cocienteComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(cocienteComplejos x y)' es el cociente de los números complejos
-- 'x' e 'y'. Por ejemplo,
--    cocienteComplejos (3,2) (1,-2)  ==  (-0.2,1.6)
-- ----------------------------------------------------------------------------

cocienteComplejos :: Complejo -> Complejo -> Complejo
cocienteComplejos (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    conjugado :: Complejo -> Complejo
-- tal que '(conjugado x)' es el conjugado del número complejo 'x'. Por
-- ejemplo,
--    conjugado (2,3)  ==  (2.0,-3.0)
-- ----------------------------------------------------------------------------

conjugado :: Complejo -> Complejo
conjugado (x1,x2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Los rectángulos pueden representarse por sus dimensiones, base
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
-- Ejercicio 20. Definir la función
--    cuadrante :: (Int,Int) -> Int
-- tal que '(cuadrante p)' es el cuadrante en el que se encuentra el punto 'p'.
-- Si el punto está sobre los ejes el resultado debe ser 0. Por ejemplo,
--    cuadrante (0,4)    ==  0
--    cuadrante (-3,0)   ==  0
--    cuadrante (0,0)    ==  0
--    cuadrante (3,5)    ==  1
--    cuadrante (-3,5)   ==  2
--    cuadrante (-3,-5)  ==  3
--    cuadrante (3,-5)   ==  4
-- ----------------------------------------------------------------------------

cuadrante :: (Int,Int) -> Int
cuadrante (x1,x2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    simetricoH :: (Int,Int) -> (Int,Int)
-- tal que '(simetricoH p)' es el punto simétrico de 'p' respecto del eje
-- horizontal. Por ejemplo,
--    simetricoH (2,5)   ==  (2,-5)
--    simetricoH (2,-5)  ==  (2,5)
-- ----------------------------------------------------------------------------

simetricoH :: (Int,Int) -> (Int,Int)
simetricoH (x1,x2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    simetricoV :: (Int,Int) -> (Int,Int)
-- tal que '(simetricoV p)' es el punto simétrico de 'p' respecto del eje
-- vertical. Por ejemplo,
--    simetricoV (2,5)   ==  (-2,5)
--    simetricoV (2,-5)  ==  (-2,-5)
-- ----------------------------------------------------------------------------

simetricoV :: (Int,Int) -> (Int,Int)
simetricoV (x1,x2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    distancia :: (Float,Float) -> (Float,Float) -> Float
-- tal que '(distancia p1 p2)' es la distancia entre los puntos 'p1' y 'p2'.
-- Por ejemplo,
--    distancia (1,2) (4,6)  ==  5.0
-- ----------------------------------------------------------------------------

distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
-- tal que '(puntoMedio p1 p2)' es el punto medio entre los puntos 'p1' y 'p2'.
-- Por ejemplo,
--    puntoMedio (0,2) (0,6)   ==  (0.0,4.0)
--    puntoMedio (-1,2) (7,6)  ==  (3.0,4.0)
-- ----------------------------------------------------------------------------

puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x1,x2) (y1,y2) = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 25. La disyunción excluyente xor de dos fórmulas se
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
-- Ejercicio 26. Definir la función 
--    xor2 :: Bool -> Bool -> Bool
-- tal que (xor2 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por
-- cada valor del primer argumento. 
-- ---------------------------------------------------------------------

xor2 :: Bool -> Bool -> Bool
xor2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función 
--    xor3 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyunción excluyente de x e y, calculada 
-- a partir de la disyunción (||), conjunción (&&) y negación (not). 
-- Usar 1 ecuación. 
-- ---------------------------------------------------------------------

xor3 :: Bool -> Bool -> Bool
xor3 x y = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función 
--    xor4 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyunción excluyente de x e y, calculada
-- a partir de desigualdad (/=). Usar 1 ecuación.
-- ---------------------------------------------------------------------

xor4 :: Bool -> Bool -> Bool
xor4 x y = undefined

-- Fuente:
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
