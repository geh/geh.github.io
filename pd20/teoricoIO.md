% Teórico Haskell: el tipo IO (input/output), la estructura de datos `Map` y la sintaxis de registro

# El tipo `IO`

## Motivación

Haskell es un lenguaje de programación funcional *puro*, es decir:

* las funciones no pueden tener efectos externos. Por ejemplo,
  una función no puede imprimir nada en la pantalla. Las
  funciones solo pueden calcular su valor de salida.
* las funciones no pueden depender de cosas externas. Por ejemplo,
  no pueden leer el teclado, o el sistema de archivos, o la red.
  Las funciones pueden solo depender de sus entradas. Dicho de
  otra forma, las funciones deben dar la misma salida para la misma entrada cada vez.

Pero… ¡a veces queremos hacer cosas así!
 
La solución a este problema es un tipo especial llamado `IO`. Los valores de tipo `IO a`
son descripciones de cálculos, que, si los ejecutamos, hacen algunas operaciones
con efectos de entrada y salida y (al final) producen un valor de tipo `a`.

Hay un nivel de indirección acá que es esencial entender. Un valor de tipo `IO a`, en sí,
es una cosa inerte. Es solo la *descripción* de un cálculo con efecto.
Una manera de pensarlo es como si fuera un programa imperativo.

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
solo un papel con algunas cosas escritas.

No solamente no tenemos una torta, sino que tenemos una receta
que no tiene ningun efecto sobre nada. Tener la receta a mano no hace
que nuestro horno se va a prender o que la harina se va a repartir
sobre la mesada, o algo por el estilo. Para efectivamente producir una
torta, la receta debe ser ejecutada.

Del mismo modo, un valor de tipo `IO a` es solo una "receta" para producir
algun valor de tipo `a` (y posiblemente tener algun efecto de entrada/salida).
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

Así que escribamos nuestro primer verdadero programa Haskell ejecutable.

Podemos usar la función:

~~~haskell
putStrLn :: String -> IO ()
~~~

que, dada una `String`, devuelve un cálculo IO que (cuando es ejecutado)
imprime esa String en la pantalla. Entonces podemos poner lo siguiente
en un archivo llamado `Hello.hs`:

~~~haskell
main = putStrLn "Hola, Haskell!"
~~~

Luego, tipear `runhaskell Hello.hs` en línea de comando resulta en nuestro mensaje
siendo impreso en la pantalla.  También podemos usar `ghc Hello.hs` para producir un
ejecutable llamado `Hello` (`Hello.exe` en Windows).

También es posible cargar el módulo en GHCi y evaluar la función `main`.

## Secuenciar acciones `IO`

Haskell provee una notación especial para hacer varias acciones `IO` consecutivas
llamada *notación do*.
 
Acá tenemos una acción que usa a notacion *do* para hacer pocas cosas:

~~~haskell
dialogoSimple :: IO ()
dialogoSimple = do
  putStrLn "Hola, usuario!"
  putStrLn "Cual es tu nombre?"
  name <- getLine
  putStrLn $ "Un gusto conocerte, " ++ name ++ "!"
~~~

Antes de ver en detalle este ejemplo, miremos un poco los tipos.
(En Haskell, siempre es útil mirar los tipos.)

Primero, empecemos por `()`. El tipo `()` se dice "unit" y tiene un
solo valor, `()`. `()` es un tipo bastante tonto
a primera vista: no transmite ninguna información, porque tiene solo
un constructor sin argumento.

Pero es exactamente lo que necesitamos en ciertas acciones `IO` que no
producen ningun valor al final. `()` es un poco como `void` en C/C++ o Java.

Veamos otros tipos:

~~~haskell
putStrLn :: String -> IO ()
getLine  :: IO String
~~~

Ya vimos el uso de `putStrLn`. Cuando secuenciamos acciones con
la notación *do* , cada linea "desnuda" (lineas que no tienen un
`<-` adentro) deben tener el tipo `IO ()`. Por suerte, `putStrLn "foo"`
tiene el tipo `IO ()`. Esas acciones se ejecutan en el orden de proceso
de un bloque *do*.

Por otro lado, `getLine` tiene como tipo `IO String`. Eso significa que
`getLine` es una acción que produce una `String`. Para conseguir la
`String` del `getLine`, usamos `<-` para dar un nombre a esa
`String`. 

Es importante ver que `name <- getLine` es solo parte de la sintaxis
de la notación *do*. No se puede incluir `name <- getLine` como parte
de una expressión más grande, sino solo como linea de un bloque *do*.

## Un ejemplo más grande

~~~haskell
jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 (lines wocky)  -- quita el titulo
  count <- printFirstLines wockylines
  putStrLn $ "Hay " ++ show count ++ " estrofas en Jabberwocky."

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

Hay varias cosas interesantes por acá:

 1. `readFile :: FilePath -> IO String`, con `type FilePath = String`.
    Esta función lee el contenido entero de un archivo y lo guarda
    dentro de una `String`.

 2. Sentencias `let` dentro de bloques *do*. La sentencia `let` en un
    bloque *do* permite crear un nombre nuevo referido a un valor.
    Observen la falta de `in`. Acordate que cuando decimos
    `let x = y`, `x` e `y` tienen el mismo tipo. Cuando decimos `x <- y`,
    `y` tiene un tipo como `IO a`, y entonces `x` tiene como tipo `a`.

 3. `return :: a -> IO a`. Si necesitamos convertir un valor puro en
    una acción IO, usamos `return`. `return` es una función común y
    corriente en Haskell. ¡No es lo mismo que `return` en C/C++ o Java!
    Dentro de una acción IO, `let x = y` es lo mismo que
    `x <- return y`, pero lo anterior es mucho mejor: hace que la pureza
    de `y` sea más obvia.

Hay muchas funciones que podemos usar para hacer entrada y salida. Fijate
en los módulos cuyo nombre empieza con `System.`, en particular `System.IO`.


## El operador `<$>`

Visualmente, es un operador que hace el paralelo entre:

* `f x` (aplicar `f` al valor puro `x`), y
* `f <$> x` (aplicar `f` al resultado del cálculo `x`).

Por ejemplo, la función `lines`, que parte una `String`
según los caracteres de fin de línea, tiene el tipo siguiente:

~~~haskell
lines :: String -> [String]
~~~

Y gracias al operador `<$>`, se puede usar de la forma siguiente:

~~~haskell
lineasDe :: FilePath -> IO [String]
lineasDe file = lines <$> readFile file
~~~

Este operador nos permite combinar más facilmente funciones puras e impuras.

# La estructura de datos `Map`

Es una estructura de diccionario, que asocia a claves algun valor.

Para utilizarlo necesitamos hacer un import del módulo `Data.Map.Strict`
(existe `Data.Map` pero es una implementación perezosa que puede causar problemas
de performancia`).

El tipo es `data Map k a` donde `k` es el tipo de las claves (key) y `a` el tipo
de los valores.

Por ejemplo `Map String String` puede representar un diccionario de palabras con
definiciones, `Map Int String` puede representar un padrón que asocia a cada DNI
el nombre del votante.

El módulo `Data.Map.String` define varias funciones sobre el tipo `Map k a`, varias
de ellas suponen que el tipo `k` pertenece a la clase `Ord`, ya que es necesario
poder ordenar las claves para lograr una complejidad $log(n)$ para insertar o
buscar elementos en el diccionario:

~~~haskell
empty :: Map k a
   -- el diccionario vacío

fromList :: Ord k => [(k, a)] -> Map k a
   -- crea un diccionario desde una lista asociativa

insert :: Ord k => k -> a -> Map k a -> Map k a
   -- inserta un valor nuevo en el diccionario, pisa el valor 

lookup :: Ord k => k -> Map k a -> Maybe a
   -- busca el valor correspondiente a la clave indicada, puede devolver Nothing
~~~

Consulta Hoogle para conocer más funciones útiles con el tipo `Map`.

# La sintaxis de registro ("record syntax")

Cuando tenemos un tipo de datos con un solo constructor pero varios
parámetros, puede llegar a ser pesado acceder a valores de dicho constructor
y modificarlos:

~~~haskell
data Transaction = Transaction String -- origen
                               String -- destino
                               Integer -- monto
                               TId     -- identificador
                   deriving (Show, Eq)


sumarMonto :: Integer -> Transaction -> Transaction
sumarMonto i (Transaction or de monto tid) =  Transaction or de (monto+i) tid
~~~

Imaginá si el tipo `Transaction` tuviera 10 parámetros en su constructor...

Por suerte existe la sintaxis de registro, que nos provee funciones para acceder
únicamente a los valores que nos interesan:

~~~haskell
data Transaction = Transaction { from   :: String
                               , to     :: String
                               , amount :: Integer
                               , tid    :: TId
                               }
                   deriving (Show, Eq)

mostrarMonto :: Transaction -> String
mostrarMonto t = show $ amount t

actualizarMonto :: Transaction -> Integer -> Transaction
actualizarMonto t i = t{amount = i}

sumarMonto :: Integer -> Transaction -> Transaction
sumarMonto i t =  t{amount = amount t + i)
~~~

# Para explorar

* [Capítulo "Entrada y Salida"](http://aprendehaskell.es/content/EntradaSalida.html)
* [la sintaxis de registro ("record syntax")](http://aprendehaskell.es/content/ClasesDeTipos.html#sintaxis-de-registro).
* [El módulo Map](http://aprendehaskell.es/content/Modulos.html#data-map)
