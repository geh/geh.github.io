# Programación funcional

# Preparando el terreno para Haskell

## Instalación/recursos

  * instalar el compilador GHC versión 8.6 por lo menos (la que trae Ubuntu 20.04)
    * `apt install ghc`
    * sino: <https://www.haskell.org/platform/>
  * buscar en la documentación/librerías: <https://hoogle.haskell.org/>
  * GHCi ayuda a ver qué hace algun código

## Errores de compilación

  * sistema de tipos fuerte: el compilador se queja, mucho
  * Haskell tiene inferencia de tipos
    * Pro: no hace falta escribir siempre los tipos
    * Contra: más difícil encontrar ubicación de un error
  * Consejos:
    1. Don't panic!
    2. Tomar los errores por uno, en orden.
    3. Intentar agregar anotaciones de tipo al código para guiar el compilador.

## Historia

  * Basado sobre el *cálculo lambda* de Alonzo Church (1930s)
  * Primer lenguaje de programación: **Lisp** de John McCarthy (1950s)
  * Popularizado por varios, en particular [John Backus](https://www.thocp.net/biographies/papers/backus_turingaward_lecture.pdf)
  * **ML** desarrollado por Robin Milner en Edinburgo (1973)
  * Miranda y **Haskell** a finales de los 1980s


## Bloques básicos: las funciones

  * Una función tiene dos componentes:
    * Entrada: argumentos pasados a la función
    * Salida: resultados de ejecutar la función
  * Las funciones son *de primera clase*: tratadas como cualquier otro valor
    * podemos pasarlas a otras funciones
    * pueden ser devueltas por otras funciones 
  * Combinar funciones para hacer funciones nuevas

## Controlas los "efectos laterales"

  * Las funciones *puras* son completamente definidas por su entrada/salida
    * siempre devuelven el mismo resultado
  * Evitan tener un *estado* escondido
    * variables locales, de conteo, etc.
  * Manejan los *efectos laterales* con cuidado, ...

*Pensarlas como programas aislados.*

# Un sorbo de Haskell

## Declarar funciones

~~~haskell
double :: Int -> Int
double n = n + n
~~~

  * primera línea: *protótipo* o *anotación de tipo* opcional
    * este dice: función de `Int` a `Int`
  * segunda línea: *definición* o *cuerpo* de la función

## Llamar funciones

* Formato: nombre de la función, espacio, argumento(s)

~~~haskell
myBool  = myFun 42    -- llamada con 42
-- NO: myBool = myFun(42) 
~~~

## Parámetros múltiples

~~~haskell
doublePlus :: Int -> Int -> Int
doublePlus x y = double x + double y
-- IGUAL QUE: doublePlus x y = (double x) + (double y)
-- PERO NO: doublePlus x y = double(x) + double(y)
~~~

* anotación de tipo: `doublePlus` toma 2 entradas
* llamadas: `double x` y `double y`

## Análisis de caso

El if-then-else clásico:

~~~haskell
doubleIfBig :: Int -> Int
doubleIfBig n = if (n > 100) then n + n else n
~~~

Más limpio (o para más casos):

~~~haskell
doubleIfBig' :: Int -> Int
doubleIfBig' n
    | n > 100   = n + n
    | otherwise = n
~~~

## Otra manera de analizar casos

Usar una expresión `case`:

~~~haskell
listPrinter''' :: [Int] -> String
listPrinter''' l = case l of
                   []     -> "Empty list :("
                   (x:xs) -> (show x) ++ " and " ++ (show xs)
~~~


## Declarar variables

Son más bien constantes en realidad.

Al principio:

~~~haskell
tripleSecret :: Int
tripleSecret = let secret = mySecretNum
                   other  = myOtherNum
               in 3 * secret + other
~~~

O al final:

~~~haskell
tripleSecret' :: Int
tripleSecret' = 3 * secret + other
                where secret = mySecretNum
                      other  = myOtherNum
~~~


# Tuplas y listas

## Construir tuplas

Las tuplas son pares, triples ...

~~~haskell
myTuple2 :: (Int, Int)
myTuple2 = (7, 42)

myTriple :: (Int, Int, Int)
myTriple = (7, 42, 108)
~~~

## Más tuplas

Las tuplas pueden mezclar tipos distintos

~~~haskell
myMixedTuple :: (Int, Int, Bool)
myMixedTuple = (7, 42, false)
~~~

La tupla vacía (0 elementos) tiene un solo valor posible:

~~~haskell
emptyTuple :: ()
emptyTuple = ()
~~~

## Trabajar con tuplas

Obtener el primer o segundo elemento:

~~~haskell
fstInt :: (Int, Int) -> Int
fstInt (x, y) = x

sndInt :: (Int, Int) -> Int
sndInt (x, y) = y

-- In standard library:
fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y
~~~

## Intercambiar elementos de una tupla

~~~haskell
swapInt :: (Int, Int) -> (Int, Int)
swapInt (x, y) = (y, x)
~~~




# Fuentes

* <https://pages.cs.wisc.edu/~justhsu/teaching/current/cs538/>
