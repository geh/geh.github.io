% Teórico: Clase `Monad`, la herramienta `cabal` 

# La clase `Monad`

## Tipos que representan cálculos

* `a` : cálculo que devuelve un `a`
* `data Maybe a = Nothing | Just a` : cálculo que puede fallar, y en caso de éxito devuelve un `a`
* `data Either a b = Left a | Right b`: cálculo que puede fallar, en cual caso devuelve un `a`
  (a menudo un mensaje de error o un valor que indica una excepción), sino devuelve un `b`
* `[a]` (lista): cálculo que devuelve cero, uno o varios valores `a` posibles
  (por razones de aleatoriedad, por ejemplo)
* `IO a`: calculo que involucra efectos de entrada y salida (leer archivos,
  usar la red, imprimir en pantalla, etc.), y devuelve un valor `a`

## Motivación de la clase `Monad`

Pensando en `IO`, ya conocemos una forma un poco limitada de secuenciar efectos `IO`,
es el uso de `<>` de la clase `Monoid`:
si tenemos una secuencia de funciones `IO a` donde `a` tambien pertenece a la clase `Monoid`,
se pueden ejecutar esas funciones en secuencia y luego combinar los resultados (con la implementación
`<>` para `a`).

Pero así no podemos:

* ejecutar secuencias de funciones `IO` que devuelven tipos distintos
* usar el resultado de una primera función IO para determinar qué hacer en la segunda
  (ej: leer contenido de un archivo, extraer de ahi el nombre de un segundo archivo para abrir) 
* bifurcar en esa secuencia en función de algun valor devuelto

## ¡`Monad` nos salva!

La clase `Monad` permite formas más poderosas de poner efectos en secuencia
(que sean Maybe, Either, IO, etc.).

Empecemos con `Maybe`, que representa cálculos que pueden fallar.

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

## Definición de la clase `Monad`

~~~haskell
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
~~~

`(>>=)` (llamado "bind") toma dos parámetros. El primero de tipo `m a`, representa un cálculo
que resulta en un valor (o varias, o ninguna) de tipo `a`, y que puede tener algun "efecto".

El segundo parámetro es una función de tipo `(a -> m b)`.
Es decir, una función que va a elegir cuál es el siguiente cómputo en función del resultado
del primero.  Es precisamente ahí que se ve la idea de la mónada de encapsular cómputos que
pueden ser puestos en secuencia.

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

¡Es distinto de `m1 <> m2`!

## Instancia de `Monad` para `Maybe`:

~~~haskell
instance Monad Maybe where
  return  = Just
  Nothing >>= _ = Nothing
  Just x  >>= k = k x
~~~

Si el primer parámetro de `(>>=)` es `Nothing`, entonces todo el cómputo falla; sino,
si es `Just x`, aplicamos el segundo parámetro a `x` para decidir qué hacer después.

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

## Uso de `let`  y `return`

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

Observaciones:

 1. La sentencia `let` en un
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

A veces se puede evitar el uso de `return`. Lo siguiente:

~~~haskell
leerReadme = do c <- readFile "README.md"
                return c
~~~

Es equivalente a:

~~~haskell
leerReadme = readFile "README.md"
~~~

## Otros usos de `Monad`

Muchas maneras de combinar cálculos de manera
parecida a lo que provee la programación imperativa,
pero con más control sobre los efectos laterales:

* parsers
* generadores de numeros aleatorios
* cómputos que acceden a un valor modificable

## `Monad` y `Functor` son parientes

* `Monad` es subclase de `Applicative` (provee `<*>`)
* `Applicative` e ssubclase de `Functor` (provee `fmap` ie `<$>`)

## Para explorar más

* el capítulo [Monad]() del libro Aprende Haskell



# El empaquetador de liberías y programas `cabal`

Creá una carpeta llamada `palabras-apellido` o `palabras-apellido1-apellido2` según haya
1 o 2 participantes al proyecto.

Usando la línea de comando, ingresá a esta carpeta y ejecutá el comando `cabal init`.

Este comando empieza un pequeño cuestionario para crear un archivo con extensión
`.cabal` que va a describir tu aplicación. Además va a crear unos archivos más,
como el `Main.hs`.

Contestá a cada una de las preguntas, y en caso de duda, siempre podés
apretar Enter para elegir la respuesta por defecto.

La única pregunta que no tiene respuesta por defecto es la siguiente:

~~~
What does the package build:
   1) Library
   2) Executable
~~~

Acá la respuesta es `2` porque vas/van a hacer un ejecutable.

Una vez que este proceso está terminado, observá los archivos que se
agregaron a la carpeta actual y leé su contenido (en particular `Main.hs`).

Comprobá que podés compilar la aplicación que creaste con los comandos siguientes:

~~~bash
cabal configure
cabal build
~~~

Podrás ejecutar la aplicación creada haciendo:

~~~bash
cabal run
Hello, Haskell!
~~~

O si hacés `cabal install`, el ejecutable se instalará en la carpeta `~/.cabal/bin`.
Configurá tu sistema para que tome en cuenta esa carpeta cuando busque un ejecutable.

Comandos útiles de cabal:

* `cabal run`    : ejecutar la aplicación
* `cabal repl`   : cargar el `Main` en `ghci`
* `cabal install`: compilar y copiar el ejecutable a `~/.cabal/bin`
* `cabal help`   : lista de comandos de `cabal`

Cuando querés usar `Data.Map.Strict` en tu aplicación `cabal` te va a decir
que necesitas agregar la librería `containers` en tu `.cabal`.
Agrega esa palabra `containers` en el campo `build-depends`. Lo mismo pasará con
otros módulos.

