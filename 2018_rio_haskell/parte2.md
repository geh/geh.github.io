---
title: El sistema de tipos de Haskell | 2/5
author: Guillaume Hoffmann
...

# Día 2

## Retomando el asunto: `Monoid`

~~~haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

instance Monoid [a] where
  mempty = []
  mappend = (++)

-- esta instancia no es estandar
instance Monoid Bool where
  mempty = False
  mappend = (||) 
~~~

`<>` es un sinónimo de `mappend`, definido en `Data.Monoid`.

~~~haskell
> :m +Data.Monoid
> "Hey" <> " " <> "you"
> False <> True <> False
~~~

## Instancias multiples, a propósito

¿Que pasa si queremos a proposito definir mas de una instancia
de algun tipo `T` en la clase `C`? Por ejemplo:

* `Monoid Int` con la operación suma y neutro 0
* `Monoid Int` con la operación producto y neutro 1

## newtype

Básicamente `data` con un solo constructor.

~~~haskell
newtype Suma = Suma Int
newtype Prod = Prod Int
~~~

Durante la compilación, se chequea el tipo, pero durante la ejecución,
`Suma` y `Prod` son iguales a `Int`, sin "overhead" como usualmente hay cuando
se crea un tipo algebráico.

Ejercicio: resolver el ejercicio del práctico sobre los monoides.

## Overlapping Instances (Instancias solapadas)

~~~haskell
instance Show a => Show (Maybe a)
instance Show (Maybe Int)
~~~

Está permitido mientras siempre hay una instancia más específica en cualquier caso.

Para permitir instancias solapadas, escribir justo después `instance`:

* `{-# OVERLAPPING #-}` en la instancia más específica
* `{-# OVERLAPPABLE #-}` en la instancia más general

Uno de los dos es suficiente.

~~~haskell
class C a where
 f :: a -> Int

instance C a => C [a] where
  ...

instance {-# OVERLAPPING  #-} C [Int] where
  ...
~~~

Ejercicio: resolver el ejercicio del práctico sobre las instancias solapadas.

## Tipos que representan cálculos

* `a` : cálculo que devuelve un `a`

* `data Maybe a = Nothing | Just a` : cálculo que puede fallar, y en caso de éxito devuelve un `a`

. . .

* `data Either a b = Left a | Right b`: cálculo que puede fallar, en cual caso devuelve un `a`
  (a menudo un mensaje de error o un valor que indica una excepción), sino devuelve un `b`

. . .

* `[a]` (lista): cálculo que devuelve un valor `a` dentro de cero, uno o varios valores `a` posibles
  (por razones de aleatoriedad, por ejemplo)

. . .

* `IO a`: calculo que involucra efectos de entrada y salida (leer archivos,
  usar la red, imprimir en pantalla, etc.), y devuelve un valor `a`

. . .

`IO` no tiene una implementación con constructores, es más bien un hack del compilador.
¡Pero como tipo se integra perfectamente al resto!

## El problema con la pureza

Haskell es un lenguaje de programación puro, es decir:

* las funciones no pueden tener efectos externos. Por ejemplo, una función
  no puede imprimir nada en la pantalla. Las funciones solo pueden calcular
  su salida.
* las funciones no pueden depender de cosas externas. Por ejemplo, no pueden
  leer el teclado, o el sistema de archivos, o la red. Las funciones pueden
  solo depender de sus entradas. Dicho de otra forma, las funciones deben
  dar la misma salida para la misma entrada cada vez.

Pero… ¡a veces queremos hacer cosas así! Si la única cosa que pudieramos hacer
con Haskell es escribir funciones para evaluarlas dentro de GHCi, sería interesante
en teoría pero inútil en práctica.

De hecho, es posible hacer ese tipo de cosas en Haskell, pero es muy distinto
a lo que se hace en otros lenguajes de programación.

## `IO` soluciona el problema con la pureza

La solución a esa enigma es un tipo especial llamado IO. Los valores de tipo `IO a`
son descripciones de cálculos con efectos, que, si los llevamos a cabo, harían
(posiblemente) algunas operaciones con efectos de entrada y salida y (al final)
producirían un valor de tipo `a`.

Hay un nivel de indirección acá que es esencial entender. Un valor de tipo `IO a`, en sí,
es una cosa inerta, totalmente pura y sin efecto. Es solo la descripción de un cálculo
con efecto. Una manera de pensarlo es como si fuera un programa imperativo.

##

Para ilustrar eso, supongamos que tenemos:

~~~haskell
t :: Torta
~~~

¿Qué tenemos? Una torta deliciosa, por supuesto. Así de simple.

En comparación, supongamos que tenemos:

~~~haskell
r :: Receta Torta
~~~

¿Qué tenemos? ¿Una torta? No, tenemos unas instrucciones para hacer una torta,
solo un papel con algunas cosas escritas encima.

No solamente no tenemos una torta, sino que estamos teniendo una receta
que no tiene ningun efecto sobre nada. Tener la receta a mano no hace
que nuestro horno se va a prender o que la harina se va a repartir
sobre la mesada, o algo por el estilo. Para efectivamente producir una
torta, la receta debe ser seguida.

##

Del mismo modo, un valor de tipo `IO a` es solo una "receta" para producir
algun valor de tipo `a` (y posiblemente tener algun efecto de paso).
Como cualquier otro valor, puede ser pasado como argumento, devuelto como
salida de una función, guardado en una estructura de datos, o (como vamos
a ver pronto) combinado con otros valores IO en recetas más complejas.

Entonces, ¿cómo los valores de tipo `IO a` terminan siendo ejecutados?
De una sola forma: el compilador Haskell busca un valor especial

~~~haskell
main :: IO ()
~~~

que va a ser entregado al runtime y ejecutado. ¡Ya está! Podemos pensar
en el runtime de Haskell como si fuera el chef que es el único con
derecho a cocinar.

Si queremos que nuestra receta sea seguida entonces debemos hacer que
sea parte de la receta grande (`main`) que es dada al chef. Por
supuesto, `main` puede ser muy complicada, y típicamente va a ser
compuesta de numerosos cálculos IO más pequeños.

##

Así que escribamos nuestro primer verdadero programa Haskell ejecutable.
Podemos usar la función:

~~~haskell
putStrLn :: String -> IO ()
~~~

que, dada una `String`, devuelve un cálculo IO que (cuando es ejecutado)
imprime esa String en la pantalla. Entonces podemos poner lo siguiente
en un archivo llamado `Hello.hs`:

~~~haskell
main = putStrLn "Hello, Haskell!"
~~~

Luego, tipear `runhaskell Hello.hs` en línea de comando resulta en nuestro mensaje
siendo impreso en la pantalla.  También podemos usar `ghc Hello.hs` para producir un
ejecutable llamado `Hello` (`Hello.exe` en Windows).

##

GHC busca un módulo llamado `Main` para encontrar la acción principal. Si no ponemos
cabezera en un archivo Haskell, el nombre por defecto será `Main`, entonces eso funciona,
por más que el archivo no se llama `Main.hs`. Si queremos usar un nombre de módulo
distinto, tenemos que usar una opción en línea de comando para ghc o runhaskell. Por
ejemplo si tenemos `Something.hs`:

~~~haskell
module Something where
main :: IO ()
main = putStrLn "Hi out there!"
~~~

Lo podemos compilar con `ghc -main-is Something Something.hs`.

## No hay una `String` "dentro de" un `IO String`

Muchos usuarios nuevos de Haskell terminan preguntando algo como
"Tengo un  `IO String`, ¿cómo lo convierto en una `String`?" o
"¿Cómo consigo la `String` dentro del `IO String`?".

Dadas las explicaciones anteriores, debería estar claro que estas
preguntas no tienen sentido: un valor de tipo `IO String` es la
descripción de algun cálculo, una receta, para generar una `String`.
No hay `String` "dentro de" un `IO String`, no más que hya una torta
"dentro de" una receta. Para producor una `String` (o una deliciosa
torta), hace falta ejecutar el cálculo (o la receta). Y la única
manera de hacerlo es darlo (posiblemente como parte de un valor
IO más grande) al runtime de Haskell, a través de `main`.

## ¿Qué significa `IO ()`?

El tipo `()` se dice "unit" y tiene un solo valor, `()`.
Es como si fuera definido con:

~~~haskell
data () = ()
~~~

Aunque no es sintaxis Haskell válida. `()` es un tipo bastante tonto
a primera vista: no transmite ninguna información, porque tiene solo
un constructor sin argumento.

Pero es exactamente lo que necesitamos en ciertas acciones IO que no
producen ningun valor al final. Haskell insiste que debe producir
algo, entonces decimos que produce `()`. (Un poco como `void` en C/C++ o Java.)

## Otras funciones de tipo `IO algo`

~~~haskell
putStrLn :: String -> IO ()
getLine  :: IO String
~~~

`putStrLn` ejecuta una acción IO pero no devuelve nada.

`getLine` tiene como tipo `IO String`. Eso significa que
`getLine` es una acción que produce una `String`.

. . .

~~~haskell
print :: Show a => a -> IO ()
readFile :: FilePath -> IO String
~~~

Tenemos `type FilePath = String`.

Esta función lee el contenido entero de un archivo y lo guarda
dentro de una `String`.

Hay muchas funciones que podemos usar para hacer entrada y salida. Fijate
en los módulos cuyo nombre empieza con `System.`, en particular `System.IO`.

## `Functor`

Consideremos el patrón de "aplicar una función pura bajo un
constructor de tipo":

~~~haskell
mapList :: (a -> b) -> List a -> List b
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapIO :: (a -> b) -> IO a -> IO b
~~~

. . .

~~~haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
~~~

## Functor, version completa

~~~haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
~~~

. . .

Leyes:

~~~haskell
fmap id  ==  id
fmap (f . g)  ==  fmap f . fmap g
~~~

Implicación: si `f` representa algun efecto (por ejemplo `IO`),
entonces aplicar una función via `fmap` no modifica ese efecto.

## El operador `<$>`

Es un sinónimo infijo de `fmap` (de la clase `Functor`)

Visualmente hace el paralelo entre:

* `f x` (aplicar `f` al valor puro `x`), y
* `f <$> x` (aplicar `f` al resultado del cálculo `x`).

Ejemplo:

~~~haskell
lineasDe :: FilePath -> IO [String]
lineasDe file = lines <$> readFile file
~~~

## La clase `Monad`: motivación

Pensando en `IO`, ya conocemos una forma un poco limitada de secuenciar efectos `IO`,
es el uso de `<>` de la clase `Monoid`:

si tenemos una secuencia de funciones `IO a` donde `a` tambien pertenece a la clase `Monoid`,
se pueden ejecutar esas funciones en secuencia y luego combinar los resultados (con la implementación
`<>` para `a`).

Pero mucho más que eso no podemos hacer. No podemos:

* ejecutar secuencias de funciones `IO` que devuelven tipos distintos
* usar el resultado de una primera función IO para determinar qué hacer en la segunda
  (ej: leer contenido de un archivo, extraer de ahi el nombre de un segundo archivo para abrir) 
* bifurcar en esa secuencia en función de algun valor devuelto

## ¡`Monad` nos salva!

La clase `Monad` permite formas más poderosas de poner efectos en secuencia
(que sean Maybe, Either, IO, etc.).

Empecemos con `Maybe`, que representa cálculos que pueden fallar.

. . .

Queremos escribir una función que "zipea" juntos dos árboles binarios
aplicándoles una función a los valores de cada nodo. Sin embargo, la
función debe fallar totalmente si los árboles tiene una estructura distinta.

~~~haskell
data Tree a = Node (Tree a) a (Tree a)
            | Empty
~~~
## `Maybe` necesita ayuda

Primer intento:

~~~haskell
data Tree a = Node (Tree a) a (Tree a)
            | Empty

zipTree1 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree1 _ (Node _ _ _) Empty = Nothing
zipTree1 _ Empty (Node _ _ _) = Nothing
zipTree1 _ Empty Empty        = Just Empty
zipTree1 f (Node l1 x r1) (Node l2 y r2) =
    case zipTree1 f l1 l2 of
      Nothing -> Nothing
      Just l  -> case zipTree1 f r1 r2 of
                   Nothing -> Nothing
                   Just r  -> Just $ Node l (f x y) r
~~~

Este código anda, pero es poco elegante. Hemos anidado dos `case`
con estructuras muy similares.

## Reduciendo la redundancia del anidamiento

Queremos una función auxiliar como la siguiente:

~~~haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe mx f = case mx of
                   Nothing -> Nothing
                   Just x  -> f x
~~~

Usando esta función, podemos tener un código más elegante:

~~~haskell
zipTree2 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree2 _ (Node _ _ _) Empty = Nothing
zipTree2 _ Empty (Node _ _ _) = Nothing
zipTree2 _ Empty Empty        = Just Empty
zipTree2 f (Node l1 x r1) (Node l2 y r2) =
    bindMaybe (zipTree2 f l1 l2) $ \l ->
        bindMaybe (zipTree2 f r1 r2) $ \r ->
            Just (Node l (f x y) r)
~~~

## Ahora sí, `Monad`

~~~haskell
class Monad m where
  return :: a -> m a

   -- pronuncado "bind"
  (>>=) :: m a -> (a -> m b) -> m b
~~~

`return` está para "envolver" un valor `a` en un valor con efectos `m a` de la forma
más trivial posible (vemos las leyes en un ratito).

`(>>=)` toma dos parámetros. El primero de tipo `m a`, representa un cálculo
que resulta en un valor (o varias, o ninguna) de tipo `a`, y que puede tener algun "efecto".

El segundo parámetro es una función de tipo `(a -> m b)`.
Es decir, una función que va a elegir cuál es el siguiente cómputo en función del resultado
del primero.  Es precisamente ahí que se ve la idea de la mónada de encapsular cómputos que
pueden ser puestos en secuencia.

##

Todo lo  que hace  `(>>=)` es poner juntas dos acciones para producir una más grande,
que lleva a cabo la primera y luego la segunda, devolviendo el resultado de la segunda.

El giro importante acá es que nos toca decidir cuál acción ejecutar en segundo en función
de la salida de la primera.

## Una función más simple de `Monad`

La clase `Monad` provee una función con una implementación por defecto:

~~~haskell
(>>)  :: m a -> m b -> m b
m1 >> m2 = m1 >>= \_ -> m2
~~~

`m1 >> m2` simplemente hace `m1` y luego `m2`, ignorando el resultado de `m1`.

¡Es bastante distinto de `m1 <> m2`!

## Leyes de `Monad`

~~~haskell
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
~~~

Leyes:

~~~haskell
return a >>= k  =  k a
m >>= return    =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h -- uf!
~~~

## Instancia de `Monad` para `Maybe`:

~~~haskell
instance Monad Maybe where
  return  = Just
  Nothing >>= _ = Nothing
  Just x  >>= k = k x
~~~

Si el primer parámetro de `(>>=)` es `Nothing`, entonces todo el cómputo falla; sino,
si es `Just x`, aplicamos el segundo parámetro a `x` para decidir qué hacer después.

De paso, es común usar la letra k para el segundo parámetro de `(>>=)` porque "k"
significa "continuación".

## Volviendo a `zipTree`

~~~haskell
zipTree3 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree3 _ (Node _ _ _) Empty = Nothing
zipTree3 _ Empty (Node _ _ _) = Nothing
zipTree3 _ Empty Empty        = Just Empty
zipTree3 f (Node l1 x r1) (Node l2 y r2) =
    zipTree3 f l1 l2 >>= \l ->
        zipTree3 f r1 r2 >>= \r ->
            return (Node l (f x y) r)
~~~

. . .

Observamos que seguimos teniendo cierto código repetido, el patrón:

~~~haskell
`... >>= \x -> ...`
~~~

Además, al anidar estas cosas, nos vamos cada vez más para la derecha.

Por eso GHC provee una notación especial: la *notación do*.

## Notación `do`

La notación `do` anda con cualquier tipo que pertenece a la clase `Monad`.

Consideramos el bloque `do` siguiente:

~~~haskell
addM :: Monad m => m Int -> m Int -> m Int
addM mx my = do
  x <- mx
  y <- my
  return $ x + y
~~~

GHC convierte esto en una version que usa `(>>=)`:

~~~haskell
addM' :: Monad m => m Int -> m Int -> m Int
addM' mx my = mx >>= \x -> my >>= \y -> return (x + y)
~~~

## `zipTree` por última vez

~~~haskell
zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ (Node _ _ _) Empty = Nothing
zipTree _ Empty (Node _ _ _) = Nothing
zipTree _ Empty Empty        = Just Empty
zipTree f (Node l1 x r1) (Node l2 y r2) = do
    l <- zipTree f l1 l2
    r <- zipTree f r1 r2
    return $ Node l (f x y) r
~~~

## La mónada de las listas

~~~haskell
instance Monad [] where
  return x = [x]
  xs >>= k = concatMap k xs
~~~

Un ejemplo simple:

~~~haskell
addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex07 = [10,20,30] >>= addOneOrTwo
ex08 = do
  num <- [10, 20, 30]
  addOneOrTwo num
~~~

Podemos pensar en la mónada de las listas como codificando cálculos con
distintos resultados posibles. Más arriba, `num` representa los valores
posibles `[10, 20, 30]` y luego es agregado a 1 o 2. El resultado es una
lista de 6 elementos con todos los resultados posibles.

Haskell tiene una sintaxis llamada listas en comprensión
(o listas intensionales), que usan de hecho la implementación
de la clase `Monad` para las listas.

## Combinadores de mónadas

Una cosa linda acerca de la clase `Monad` es que sólo usando `return` y `(>>=)`
podemos construir muchos combinadores generales para programas con mónadas.

Primero, `sequence` toma una lista de valores monádicos y produce un solo valor
monádico que colecta los resultados. Lo que significa realmente depende de cada
mónada. Por ejemplo, en el caso de `Maybe`, significa que el cómputo general
es exitoso solo si todos los cómputos individuales lo son; en el caso de IO
significa que ejecuta todos los cómputos en secuencia.

~~~haskell
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) = do
  a  <- ma
  as <- sequence mas
  return (a:as)
~~~

## Más combinadores

Usando `sequence`, uno puede escribir otros combinadores, como:

~~~haskell
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

void :: Monad m => m a -> m ()
void ma = ma >> return ()

join :: Monad m => m (m a) -> m a
join mma = do
  ma <- mma
  ma

when :: Monad m => Bool -> m () -> m ()
when b action =
  if b
  then action
  else return ()
~~~

## Más sobre la notación `do`

Ahora esto debería tener sentido:

~~~haskell
sillyExchange :: IO ()
sillyExchange = do
  putStrLn "Hello, user!"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"
~~~

## Uso de `let` en `do`

~~~haskell
jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 (lines wocky)  -- quita el titulo
  count <- printFirstLines wockylines
  putStrLn $ "There are " ++ show count ++ " stanzas in Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines ls = do
  let first_lines = extractFirstLines ls
  putStr (unlines first_lines)
  return $ length first_lines

extractFirstLines :: [String] -> [String]
extractFirstLines []         = []
extractFirstLines [_]        = []
extractFirstLines ("" : first : rest)
  = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest
~~~

## Observaciones

 1. Sentencias `let` dentro de bloques *do*. La sentencia `let` en un
    bloque *do* permite crear un nombre nuevo atado a un valor puro.
    Observen la falta de `in`. Acuérdense que cuando decimos
    `let x = y`, `x` e `y` tienen el mismo tipo. Cuando decimos `x <- y`,
    `y` tiene un tipo como `IO a`, y entonces `x` tiene como tipo `a`.

 2. `return :: a -> IO a`. Si necesitamos convertir un valor puro en
    una acción IO, usamos `return`. `return` es una función común y
    corriente en Haskell. ¡No es lo mismo que `return` en C/C++ o Java!
    Dentro de una acción IO, `let x = y` es lo mismo que
    `x <- return y`, pero lo anterior es mucho mejor: hace que la pureza
    de `y` sea más obvia.

Otra forma de verlo es que que encontramos a menudo `return` como última
línea de un bloque `do` porque cada línea debe ser del tipo `IO`.

##

Ojo, lo siguiente:

~~~haskell
leerReadme = do c <- readFile "README.md"
                return c
~~~

Es equivalente a (y peor que):

~~~haskell
leerReadme = readFile "README.md"
~~~


## Otros usos de `Monad`

* parsers
* generadores de numeros aleatorios
* cómputos que acceden a un estado mutable
* etc.

## Una clase intermedia: `Applicative`

`Applicative` es una clase que está "entre" `Functor` y `Monad`.

~~~haskell
class (Functor f) => Applicative f where  
  pure :: a -> f a  
  (<*>) :: f (a -> b) -> f a -> f b  
~~~

* `pure` es exactamente el `return` de `Monad`.
* `<*>` generaliza `fmap` pero no es tan poderoso cono `>>=`.

A veces la potencia de `Monad` es demasiado, y `Applicative` basta.

Ver <https://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Applicative.html>

# Conclusión

## Ventajas de las clases de tipos

* Sobrecarga
* Resolución de instancias
* Leyes
* Algoritmos genéricos
* Coherencia: una sola instancia por par (clase,tipo)

## Ultimo consejo

Tener a mano:

* Typeclasseopedia: <https://wiki.haskell.org/Typeclassopedia>
* GHC User Guide: <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>
* <http://dev.stephendiehl.com/hask/#eightfold-path-to-monad-satori>
