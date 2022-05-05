% Prog. Declarativa: Parcial 2
% 2019-11-13


Resolvé los ejercicios en un módulo que tenga como nombre tu primer
apellido, con extensión `.hs`.

El módulo es a mandar a <guillaume.hoffmann@conicet.gov.ar>.

Se tomará en cuenta que se use lo más posible el estilo Haskell.
La consignas son:

* usar pattern-matching donde se pueda
* no hacer comparaciones si se puede hacer pattern matching
* no hacer guardas si se puede hacer pattern matching
* usar guardas en lugar de comparaciones con if-then-else
* no usar funciones parciales sobre listas si se puede hacer
  pattern-matching (no usar `head`, `tail`, etc.)

Condiciones de regularidad:

*  tener por lo menos 3 puntos entre los ejercicios 1 y 2

# Ejercicio 1 (2 puntos)

1. Implementá `describeI :: Integer -> String`
   que toma un entero $n$
   y devuelve una cadena que lo describe, siguiendo las reglas
   siguientes:
   * si $n == 0$ devolver "cero"
   * si $n == 1$ devolver "uno"
   * si $n < 0$ devolver "negativo"
   * para los otros valores, devolver "mucho"

2. Implementá `describe :: [Integer] -> String` que
   devuelve una sola String con cada palabra separada por un espacio
   (podés usar `map` y `unwords`).

~~~haskell
describe [5,2,-3,-2] == "mucho mucho negativo negativo"
~~~

3. Implementá la función `bueno :: Integer -> Bool`, que
   toma un entero $n$ como entrada y devuelve:
   * si $n < -50$, `False`
   * sino, si $n > 80$, `False`
   * sino, devuelve `True`

4. Implementá la función `buenos :: [Integer] -> [Integer]` que
   toma una lista de enteros y devuelve solo los elementos
   que cumplen con la función `bueno`, usando `filter`.

~~~haskell
buenos [16,-100,299,56,5,4] == [16,56,5,4]
~~~

# Ejercicio 2:  el tipo `Maybe` (2 puntos)

Definí las funciones siguientes:

* `div_segura :: Int -> Int -> Maybe Int`
  que toma dos enteros y devuelve `Nothing` si el segundo vale cero, o `Just c` donde `c` es el cociente del primero por el segundo.
* `cabeza :: [a] -> Maybe a`
  que toma una lista y devuelve el primer elemento (dentro de `Just`) solamente si la lista no es vacía, y `Nothing` si es vacía.
* `suma :: Maybe Int -> Maybe Int -> Maybe Int`
  que toma dos "enteros Maybe", y solo devuelve un `Just` de la suma si los dos son `Just`. En los otros casos, devuelve `Nothing`.
* `mMap :: Maybe (a -> a) -> [a] -> [a]`
  que toma, tal vez, una función, y una lista. Si la función es en realidad Nothing devuelve la
  misma lista, sino aplica la función a la lista y devuelve ese resultado (se puede usar `map`).


# Ejercicio 3: definiciones de funciones por casos (2 puntos)

1. Programá las siguientes funciones, usando caso base e inductivo (**sin** usar `map`, `filter`, `fold`):
    * `paratodo :: [Bool] -> Bool` que  verifica  que todos los elementos de una lista
      sean `True`.
    * `sumatoria :: [Int] -> Int` que devuelve la suma de todos los elementos de la lista.

      Ejemplos de uso:

~~~haskell
> paratodo [True, False, True]
False
> paratodo [True, True]
True
> sumatoria [1, 5, -4]
2
~~~

2. Programar la función `pertenece :: Int -> [Int] -> Bool`
   que verifica si un numero se encuentra o no en la lista. Definirla usando recursion, con caso base e inductivo.

   Ejemplos:

~~~haskell
> pertenece 4 [2,4,6]
True
> pertenece 6 [2,4,6]
True
> pertenece 7 [2,4,6]
False
~~~

3. Considerá las siguientes funciones:
    * `sumarALista :: Num a => a -> [a] -> [a]` que  toma  un  numero  y  una  lista  de
      numeros y le suma a cada elemento de la lista el primer parametro. Por ejemplo:
      `sumarALista 3 [4,6,7] = [7,9,10]`.
    * `encabezar :: a -> [[a]] -> [[a]]` que toma una expresion de tipo `a` y lo pone en
      la cabeza de cada lista del segundo parametro. Por ejemplo:
      `encabezar 3 [[2,1],[],[4,7]] = [[3,2,1],[3],[3,4,7]]`
    * Escribí `sumarALista` y `encabezar` usando caso base y caso inductivo.
    * Escribí ambas funciones utilizando la función `map` (dales un nombre distinto)

4. Considerá la funcion `encuentra` que dado un valor de tipo `Int`
   y  una  lista  de  pares `[(Int,String)]` devuelve el segundo componente del primer par
   cuyo primer componente es igual al primer parametro. En el caso que ningun elemento
   de la lista cumpla con esto, devolver el string vacío. Por ejemplo:

~~~haskell
encuentra 10 [(40,"tos"),(10,"uno"),(16,"taza"),(10,"dos")] = "uno"
encuentra 102 [(40,"tos"),(103,"vela"),(16,"taza")] = ""
encuentra 102 [] = ""
~~~

Definir la funcion en forma recursiva pensando el caso base y caso inductivo.

# Ejercicio 4: Picos (1 punto)

Definimos un *pico* de una lista como un elemento de la lista que
es estrictamente mayor a los dos elementos que lo rodean.
Por ejemplo en la lista `[2,3,4,1,5]` el único pico es `4`.
`5` no es un pico porque no tiene elemento después de él.

Escribí la función:

~~~haskell
picos :: [Integer] -> [Integer]
~~~

Que encuentra todos los picos de la lista de entrada y los
devuelve en el orden. Por ejemplo

~~~haskell
picos [2,9,5,6,1] == [9,6]
picos [2,3,4,1,5] == [4]
picos [1,2,3,4,5] == []
~~~

# Ejercicio 5: `IO` (1 punto)

Copiá y pegá el contenido siguiente en un archivo `numeros.txt`:

~~~
30 2 1 33 31 14
1 41 44
4 9 87
121 7 2 4 641
~~~

Definí la función

~~~haskell
leeNumeros :: FilePath -> IO [[Int]]
~~~

que toma el camino a ese archivo y devuelve su contenido como lista de listas.
Podrás usar las funciones `words`, `lines`, `read` y `map`.

# Ejercicio 6: Tipo de datos algebraico, instancias de clases (2 puntos)

Definí:

~~~haskell
data Conjunto = C [Int]
~~~

Un tipo de datos que representa conjuntos de enteros como lista (es decir,
con muy mala performancia).

 1. Implementa la instancia `Show Conjunto` tal que se muestre así:

    ~~~haskell
    > show (C [1,2,3])
    "{1,2,3}"
    C [1,2,3]
    {1,2,3}
    ~~~

    Para eso podés utilizar la función `intercalate` del módulo `Data.List`.

    Se mostrará simplemente el contenido de la lista sin tratar de ordenar
    o eliminar duplicados

    ~~~haskell
    C [10,10,1,2,2,3]
    {10,10,1,2,2,3}
    ~~~


 2. Implementá la instancia `Eq Conjunto` que compara si dos conjuntos
    son iguales. Ahora sí consideramos iguales dos `Conjunto` si
    los conjuntos construidos a partir de sus respectivas listas
    son iguales:

    ~~~haskell
    C [10,10,1,2,2,3] == C [1,3,2,10]
    True
    ~~~

    Para eso podés utilizar la función `fromList` del módulo `Data.Set`.

 3. Implementá la función que da el máximo elemento de un conjunto:

    ~~~haskell
    maxC :: Conjunto -> Maybe Int
    ~~~

    ~~~
    > maxC $ C [10,10,1,2,2,3]
    Just 10
    > maxC $ C []
    Nothing
    ~~~

 4. Definí una instancia `Arbitrary Conjunto` para poder generar Conjuntos
    aleatoriamente.

    ~~~haskell
    > sample (arbitrary :: Gen Conjunto)
    {}
    {-2}
    {}
    {}
    {6,7,5,-2}
    {-5,0,2}
    {5,-11,-11,-1,6,-12,-1,-6,5,-10,5,-8}
    {2,-10,7,-5,-2,11,7,-9}
    {16,5}
    {15}
    {-10,-18,-13,-11,-8,2,-6,-14,-4,-1,-18,13,4,1,-11,-3,-11,4}
    ~~~

 5. Definí una propiedad `prop_maxC` para QuickCheck que dice que si dos conjuntos
    son iguales, entonces sus máximos son iguales.

    Probá con `quickCheck prop_maxC`.
