% Teórico Haskell: QuickCheck

Antes que nada:

~~~bash
cabal install QuickCheck
~~~

# QuickCheck 

## Una función para comprobar

Supongamos que queremos fusionar dos listas _ordenadas_.

~~~haskell
type MergeFun = Ord a => [a] -> [a] -> [a]
merge :: MergeFun
merge all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge all_ys xs
  | otherwise = y : merge all_xs ys
merge xs            []            = xs
merge _             _             = []
~~~

¿Esta función anda como lo esperamos? Podríamos probarla unas veces en
GHCi, pero es un poco insatisfactorio: tenemos que hacer el trabajo de
pensar en un test, pero lo usamos solo una vez. En lugar de eso, es mucho
mejor escribir el test dentro del archivo, de tal forma que lo puedamos
volver a ejecutar cada vez que modificamos la función `merge`.

QuickCheck provee formas de testear nuestros programas. Se
enfoca en *tests unitarios*, donde solo una pequeña parte de un programa
es escrutinada, en búsqueda de problemas.
 
## QuickCheck: tests basados en propiedades

Es aburrido escribir casos de tests. Y es fácil olvidarse de un
comportamiento inesperado. Mucho mejor sería definir *propiedades*
que queramos que nuestra función tenga.
Luego, podríamos hacer que la computadora genere los casos de tests
para nosotros.

QuickCheck es la librería estándar en Haskell para hacer tests
basados en propiedades. La idea es que definís alguna *propiedad*,
que luego se chequea usando datos pseudoaleatorios.

Por ejemplo:

~~~haskell
prop_numElements_merge :: [Integer] -> [Integer] -> Bool
prop_numElements_merge xs ys
  = length xs + length ys == length (merge3 xs ys)
~~~

Esta propiedad dice que la suma de los tamaños de las listas de entrada
debería ser la misma que el tamaño de la lista de salida.
(Es costumbre empezar los nombres de propiedades con `prop_`.)
¡Probemos!

    *Main> quickCheck prop_numElements_merge
    *** Failed! Falsifiable (after 5 tests and 4 shrinks): 
    []
    [0]

(Tu resultado puede ser un poco distinto.  Acordate que está
usando aleatoriedad.)

La primera cosa que observamos es que nuestra función está claramente
equivocada. Luego vemos que QuickCheck hizo 5 tests antes de descubrir
un caso de tests fallando, entonces nuestra función no es tan mala.
QuickCheck nos dice que los parámetros que fallan son `[]` y `[0]`.
Efectivamente, GHCi nos dice que `merge3 [] [0]` es `[]`, lo que es
falso.

Lo lindo acá es que QuickCheck nos encontró un caso de test pequeño que
demuestra que nuestra función está equivocada. La manera de hacerlo es
que usa una técnica llamada *shrinking* (encogimiento). Despúes de
encontrar un caso de test que causa una falla, QuickCheck intenta buscando
parámetros cada vez más pequeños que sigan produciendo una falla.
Es maravilloso, porque sino QuickCheck nos generaría casos inmanejables
y difíciles de tomar en cuenta.

Una nota final sobre esta propiedad es que la signatura de tipo nos dice
que la propiedad toma listas de enteros, no cualquier tipo `a`. Esto es
para que GHC no elija un tipo tonto, como `()`. Debemos siempre
tener cuidado con eso cuando escribimos propiedades sobre funciones
polimórficas. Los números son siempre una buena elección.

### Implicaciones

Como lo hicimos arriba, generalicemos nuestro tests sobre implementaciones
de nuestra operación de fusión. Otra vez, probablemente no sea necesario
que lo hagas.

~~~haskell
prop_numElements :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_numElements merge xs ys
  = length xs + length ys == length (merge xs ys)
~~~

E intentamos otra vez con nuesta función:

~~~haskell
merge2 :: MergeFun
merge2 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge2 xs all_ys
  | otherwise = y : merge2 all_xs ys
merge2 xs            ys            = xs ++ ys
~~~

    *Main> quickCheck (prop_numElements merge4)
    +++ OK, passed 100 tests.

¡Huzzah!

¿Ya está? ¿Hemos terminado? Todavía no. Intentemos otra propiedad:

~~~haskell
prop_sorted1 :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_sorted1 merge xs ys
  = merge xs ys == sort (xs ++ ys)
~~~

    *Main> quickCheck (prop_sorted1 merge2)
    *** Failed! Falsifiable (after 4 tests and 3 shrinks): 
    []
    [1,0]

Rayos. QuickCheck, razonablemente, intentó con la lista `[1,0]`.
Por supuesto, no va a funcionar porque no está ordenada.
Necesitamos especificar una propiedad de implicación:

~~~haskell
prop_sorted2 :: MergeFun -> [Integer] -> [Integer] -> Property
prop_sorted2 merge xs ys
  = isSorted xs && isSorted ys ==> merge xs ys == sort (xs ++ ys)

isSorted :: Ord a => [a] -> Bool
isSorted (a:b:rest) = a <= b && isSorted (b:rest)
isSorted _          = True    -- tiene menos de 2 elementos
~~~

En `prop_sorted`, vemos que usamos el operador `(==>)`. Su tipo es
`Testable prop => Bool -> prop -> Property`. (Es un `Testable` *distinto*
del de HUnit.) Toma un `Bool` y un `Testable` y produce un `Property`.
Observá como `prop_sorted` devuelve un `Property`, no un `Bool`.
Vamos a poner orden en estos tipos más adelante, pero es interesante
la aparición de `Property` acá.

Veamos como esto funciona:

    *Main> quickCheck (prop_sorted2 merge2)
    *** Gave up! Passed only 21 tests.

(Eso tomo capaz 20 segundos.) No hay ninguna falla, pero no hay muchos éxitos
tampoco. El problema es que QuickCheck va a ejecutar el test solamente cuando
las dos listas generadas aleatoriamente de tamaño `n` son ordenadas. Pero
la probabilidad de que eso pase es de `1/n!`, lo que es, en general, muy bajo.
Y además necesitamos *dos* listas ordenadas. Eso no va a funcionar bien.

### Los tipos de `QuickCheck`

¿Cómo hace QuickCheck para generar los casos arbitrarios? Usa la clase `Arbitrary`:

~~~haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink    :: a -> [a]
~~~

Dejemos `shrink` a la documentación en línea, y enfoquemonos en `arbitrary`.
La función `arbitrary` nos da un `Gen a` - un generador para el tipo `a`.
Por supuesto, a la función `arbitrary` para listas no le importa el ordenamiento
(de hecho, no le *puede* importar, dada la parametricidad), pero a nosotros sí.
Por suerte, es un problema común, y QuickCheck provee una solución bajo la forma
de un `OrderedList`, un envoltorio alrededor de listas que tiene la instancia
`Arbitrary` que necesitábamos:

~~~haskell
newtype OrderedList a = Ordered { getOrdered :: [a] }
instance (Ord a, Arbitrary a) => Arbitrary (OrderedList a) where ...
~~~

(`newtype` es casi igual a `data`. Buscá en línea para más información.)

Ahora, reescribamos nuestra propiedad:

~~~haskell
prop_sorted3 :: MergeFun
             -> OrderedList Integer -> OrderedList Integer -> Bool
prop_sorted3 merge (Ordered xs) (Ordered ys)
  = merge xs ys == sort (xs ++ ys)
~~~

    *Main> quickCheck (prop_sorted3 merge4)
    +++ OK, passed 100 tests.

¡Huzzah! Solo cambiando un poco los tipos, podemos afectar la selección
de la instancia para conseguir lo que queríamos.

Sí, todo esto parece magia negra. ¿Cómo hace QuickCheck? Veamos más en detalle
los tipos.

~~~haskell
quickCheck :: Testable prop => prop -> IO ()

class Testable prop where ...
instance Testable Bool where ...
instance Testable Property where ...
instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where ...
~~~

Podemos `quickCheck`-ear cualquier cosa que sea `Testable`. Los valores
Booleanon son `Testable`, tanto como el misterioso `Property`. Pero es la
última instancia de `Testable` que nos llama la atención. Dice que una
*function* es `Testable` siempre y cuando su parámetro tiene una función
`arbitrary`, su parámetro puede ser mostrado (en caso de falla) y el restultado
es `Testable`.

¿Es `Testable`  `[Integer] -> [Integer] -> Bool`? Por supuesto. Recordá que
`[Integer] -> [Integer] -> Bool` es equivalente a `[Integer] -> ([Integer] -> Bool)`.
Porque `[Integer]` tiene las instancias para `Arbitrary` y `Show`, podemos usar
la última instancia más arriba siempre y cuando `[Integer] -> Bool` es `Testable`.
Y eso es `Testable` porque tenemos (otra vez) una instancia `Arbitrary` y `Show`
para `[Integer]`, y `Bool` es `Testable`. Entonces, así es como `quickCheck`
trabaja - utiliza las instancias de `Arbitrary` para los tipos de los parámetros.
Y, así es como cambiar los tipos de argumentos de `OrderedList Integer` nos produjo
el resultado que queríamos.

### Generando datos arbitrarios

Cuando querés usar QuickCheck sobre tus propios tipos de datos, es necesario escribir
una instancia `Arbitrary` para ellos. Ahora, veamos como hacerlo.

Supongamos que tenemos un tipo personalizado de listas:

~~~haskell
data MyList a = Nil | Cons a (MyList a)

instance Show a => Show (MyList a) where
  show = show . toList
  
toList :: MyList a -> [a]
toList Nil           = []
toList (a `Cons` as) = a : toList as

fromList :: [a] -> MyList a
fromList []     = Nil
fromList (a:as) = a `Cons` fromList as
~~~

Si queremos una instancia para `Arbitrary`, debemos definir la función `arbitrary`,
de tipo `Gen (MyList a)`. Por suerte para nosotros, `Gen` es una mónada,
asi que algunos detalles ya son familiares. Tambíen nos damos cuenta que si
queremos una lista arbitraria de `a`, vamos a necesitar generar valores `a`
arbitrarios. Entonces, nuestra instancia se parece a:

~~~haskell
instance Arbitrary a => Arbitrary (MyList a) where
  arbitrary = genMyList1 
~~~

A esta algura, es necesario chequear los combinadores disponibles en
la sección "Generator combinators" de [la documentación de
QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck.html).

Es útil pensar cómo vos, un ser humano, haría para generar una lista arbitraria.
Una manera de hacerlo es elegir un tamaño arbitrario (por ejemplo, entre 0 y 10),
y luego elegir cada elemento de manera arbitraria. Acá tenemos una
implementación:

~~~haskell
genMyList1 :: Arbitrary a => Gen (MyList a)
genMyList1 = do
  len <- choose (0, 10)
  vals <- replicateM len arbitrary
  return $ fromList vals
~~~

Probamos:

    *Main> sample genMyList1
    [(),(),(),(),(),()]
    []
    [(),(),(),(),(),(),(),(),()]
    [(),(),(),(),(),()]
    [(),(),(),(),(),(),(),(),()]
    [()]
    [(),(),(),(),(),(),(),()]
    [(),(),(),(),(),(),(),(),()]
    [(),(),()]
    [(),(),(),(),(),()]
    [(),(),(),(),(),(),(),(),(),()]

Los tamaños arbitrarios funcionan, pero la generación de elementos luce muy
aburrida. Usemos una anotación de tipo para hacerlo más interesante!

    *Main> sample (genMyList1 :: Gen (MyList Integer))
    [0,0,0,0,0,0,0,0,0,0]
    []
    [-2,3,1,0,4,-1]
    [-5,0,2,1,-1,-3]
    [-5,-6,-7,-2,-8,7,-3,4,-6]
    [4,-3,-3,2,-9,9]
    []
    [10,-1]
    [9,-7,-16,3,15]
    [0,14,-1,0]
    [3,18,-13,-17,-20,-8]

Mucho mejor.

Pero esta generación todavia no es genial, porque puede ser que una función escrita
para `MyList` falle solo con listas mayores a 10 elementos. Deseamos tamaños sin límites.
Hay una manera de hacerlo:

~~~haskell
genMyList2 :: Arbitrary a => Gen (MyList a)
genMyList2 = do
  make_nil <- arbitrary
  if make_nil    -- type inference tells GHC that make_nil should be a Bool
     then return Nil
     else do
       x <- arbitrary
       xs <- genMyList2
       return (x `Cons` xs)
~~~

    *Main> sample (genMyList2 :: Gen (MyList Integer))
    [0,0,0,0,0,0]
    []
    [3,-3]
    []
    []
    [-1,-1]
    [-10]
    []
    []
    [11]
    [-20,-14]

Los tamaños no tienen límite (me vas a tener que creer), pero estamos consiguiendo
*muchas* listas vacías. Eso es porque en cada nodo de una lista, tenemos un 50% de
probabilidad de producir  `Nil`. Eso significa que una lista de tamaño `n` va a
aparecer cada `2^n` veces. Entonce, los tamaños no son acodatos pero son muy poco
probables.

La forma de progresar acá es de utilizar el combinador `sized`.
QuickCheck está configurado para probar con cosas arbitrarias "simples" antes
de "complejas". La manera de hacerlo es usando un parámetro de tamaño, interno
a la mónada `Gen`. Cuanto más generador es QuickCheck, más alto se vuelve ese
parámetro. Queremos usar el parámetro de tamaño para hacer nuestra generación.

Miremos el tipo de `sized`:

~~~haskell
sized :: (Int -> Gen a) -> Gen a
~~~

La mejor forma de explicar como funciona es con un ejemplo:

~~~haskell
genMyList3 :: Arbitrary a => Gen (MyList a)
genMyList3 = sized $ \size -> do
  len <- choose (0, size)
  vals <- replicateM len arbitrary
  return $ fromList vals
~~~

    *Main> sample (genMyList3 :: Gen (MyList Integer))
    []
    [-2]
    [-1,3,4]
    [-4,-2,1,-1]
    []
    []
    [12,3,11,0,3,-12,10,5,11,12]
    [-4,-8,-9,2,14,5,8,11,-1,7,11,-8,2,-6]
    [6,10,-5,15,6]
    [-3,-18,-4]
    [9,19,13,-19]

Esto anduvo lindo - las listas tienden a volverse más larga a medida que aparecen
tarde. La idea es que `sized` toma una *continuación*: la cosa que hay que hacer
con el parámetro. Solo usamos una función lambda como argumento de `sized`.
Si es demasiado complicado (por ejemplo solo queremos producir el parámetro de
tamaño, sin usar una continuación), siempre podés hacer algo como lo siguiente:

~~~haskell
getSize :: Gen Int
getSize = sized return
~~~

Te dejo entender vos mismo como esto funciona. ¡Seguí los tipos!

### Más ejemplos con QuickCheck

* <http://www.glc.us.es/~jalonso/vestigium/i1m2013-verificacion-de-la-ordenacion-por-mezcla-con-quickcheck/>
* <http://www.glc.us.es/~jalonso/vestigium/i1m2016-estadistica-descriptiva-en-haskell/>
* <http://www.glc.us.es/~jalonso/vestigium/i1m2015-ejercicios-del-cifrado-cesar/>

