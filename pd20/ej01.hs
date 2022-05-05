-- Definiciones por composici�n sobre n�meros, listas y booleanos. 
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se plantean ejercicios con definiciones de funciones 
-- por composici�n sobre n�meros, listas y booleanos.
-- 
-- Para solucionar los ejercicios puede ser �til el manual de
-- funciones de Haskell que se encuentra en http://bit.ly/1uJZiqi y su
-- resumen en http://bit.ly/ZwSMHO

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n media3 tal que (media3 x y z) es
-- la media aritm�tica de los n�meros x, y y z. Por ejemplo, 
--    media3 1 3 8     ==  4.0
--    media3 (-1) 0 7  ==  2.0
--    media3 (-3) 0 3  ==  0.0
-- ---------------------------------------------------------------------

media3 x y z = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n sumaMonedas tal que 
-- (sumaMonedas a b c d e) es la suma de los pesos correspondientes a 
-- a monedas de 1 pesos, b de 2 pesos, c de 5 pesos, d 10 pesos y
-- e de 20 pesos. Por ejemplo,
--    sumaMonedas 0 0 0 0 1  ==  20
--    sumaMonedas 0 0 8 0 3  == 100
--    sumaMonedas 1 1 1 1 1  ==  38
-- ---------------------------------------------------------------------

sumaMonedas a b c d e = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n volumenEsfera tal que 
-- (volumenEsfera r) es el volumen de la esfera de radio r. Por ejemplo,
--    volumenEsfera 10  ==  4188.790204786391
-- Indicaci�n: Usar la constante pi.
-- ---------------------------------------------------------------------

volumenEsfera r = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n areaDeCoronaCircular tal que 
-- (areaDeCoronaCircular r1 r2) es el �rea de una corona circular de
-- radio interior r1 y radio exterior r2. Por ejemplo,
--    areaDeCoronaCircular 1 2 == 9.42477796076938
--    areaDeCoronaCircular 2 5 == 65.97344572538566
--    areaDeCoronaCircular 3 5 == 50.26548245743669
-- ---------------------------------------------------------------------

areaDeCoronaCircular r1 r2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n ultimaCifra tal que (ultimaCifra x)
-- es la �ltima cifra del n�mero x. Por ejemplo,
--    ultimaCifra 325  ==  5
-- Indicaci�n: Usar la funci�n rem
-- ---------------------------------------------------------------------

ultimaCifra x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n maxTres tal que (maxTres x y z) es
-- el m�ximo de x, y y z. Por ejemplo,
--    maxTres 6 2 4  ==  6
--    maxTres 6 7 4  ==  7
--    maxTres 6 7 9  ==  9
-- Indicaci�n: Usar la funci�n max.
-- ---------------------------------------------------------------------

maxTres x y z = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n rota1 tal que (rota1 xs) es la lista
-- obtenida poniendo el primer elemento de xs al final de la lista. Por
-- ejemplo, 
--    rota1 [3,2,5,7]  ==  [2,5,7,3]
-- ---------------------------------------------------------------------

rota1 xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n rota tal que (rota n xs) es la lista
-- obtenida poniendo los n primeros elementos de xs al final de la
-- lista. Por ejemplo, 
--    rota 1 [3,2,5,7]  ==  [2,5,7,3]
--    rota 2 [3,2,5,7]  ==  [5,7,3,2]
--    rota 3 [3,2,5,7]  ==  [7,3,2,5]
-- ---------------------------------------------------------------------

rota n xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n rango tal que (rango xs) es la
-- lista formada por el menor y mayor elemento de xs.
--    rango [3,2,7,5]  ==  [2,7]
-- Indicaci�n: Se pueden usar minimum y maximum.
-- ---------------------------------------------------------------------

rango xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n palindromo tal que (palindromo xs) se
-- verifica si xs es un pal�ndromo; es decir, es lo mismo leer xs de
-- izquierda a derecha que de derecha a izquierda. Por ejemplo,
--    palindromo [3,2,5,2,3]    ==  True
--    palindromo [3,2,5,6,2,3]  ==  False
-- ---------------------------------------------------------------------

palindromo xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la funci�n interior tal que (interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs. Por ejemplo,
--    interior [2,5,3,7,3]  ==  [5,3,7]
--    interior [2..7]       ==  [3,4,5,6]
-- ---------------------------------------------------------------------

interior xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la funci�n finales tal que (finales n xs) es la
-- lista formada por los n finales elementos de xs. Por ejemplo,
--    finales 3 [2,5,4,7,9,6]  ==  [7,9,6]
-- ---------------------------------------------------------------------
 
finales n xs = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la funci�n segmento tal que (segmento m n xs) es
-- la lista de los elementos de xs comprendidos entre las posiciones m y
-- n. Por ejemplo,
--    segmento 3 4 [3,4,1,2,7,9,0]  ==  [1,2]
--    segmento 3 5 [3,4,1,2,7,9,0]  ==  [1,2,7]
--    segmento 5 3 [3,4,1,2,7,9,0]  ==  []
-- ---------------------------------------------------------------------
 
segmento m n xs = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la funci�n extremos tal que (extremos n xs) es
-- la lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs. Por ejemplo, 
--    extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  ==  [2,6,7,9,2,3]
-- ---------------------------------------------------------------------

extremos n xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la funci�n mediano tal que (mediano x y z) es el
-- n�mero mediano de los tres n�meros x, y y z. Por ejemplo,
--    mediano 3 2 5  ==  3
--    mediano 2 4 5  ==  4
--    mediano 2 6 5  ==  5
--    mediano 2 6 6  ==  6
-- Indicaci�n: Usar maximum y minimum.
-- ---------------------------------------------------------------------
 
mediano x y z = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la funci�n tresIguales tal que 
-- (tresIguales x y z) se verifica si los elementos x, y y z son
-- iguales. Por ejemplo, 
--    tresIguales 4 4 4  ==  True
--    tresIguales 4 3 4  ==  False
-- ---------------------------------------------------------------------

tresIguales x y z = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la funci�n tresDiferentes tal que 
-- (tresDiferentes x y z) se verifica si los elementos x, y y z son
-- distintos. Por ejemplo, 
--    tresDiferentes 3 5 2  ==  True
--    tresDiferentes 3 5 3  ==  False
-- ---------------------------------------------------------------------

tresDiferentes x y z = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la funci�n cuatroIguales tal que 
-- (cuatroIguales x y z u) se verifica si los elementos x, y, z y u son
-- iguales. Por ejemplo, 
--    cuatroIguales 5 5 5 5   ==  True
--    cuatroIguales 5 5 4 5   ==  False
-- Indicaci�n: Usar la funci�n tresIguales.
-- ---------------------------------------------------------------------

cuatroIguales x y z u = undefined


-- Fuente:
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
