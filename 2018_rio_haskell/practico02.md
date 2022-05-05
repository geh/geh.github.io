% Práctico Haskell 2: Monoid, IO, Functor, Aplicative, Monad

# Monoid 

En el teórico hemos hablado del que un tipo como `Int` puede
tener ser instancia de la clase `Monoid` de dos maneras distintas.

Descargar y resolver [MonoidInt.hs](MonoidInt.hs).

# Instancias solapadas

Descargar y resolver [Solapamiento.hs](Solapamiento.hs)

Vamos a tener que usar la extensión `FlexibleInstances`.

Agregar `{-# LANGUAGE FlexibleInstances #-}` al principio del archivo.

Permite que la cabeza de una declaración de instancia mencione tipos
anidados arbitrarios:

~~~haskell
instance C (Maybe Int)
~~~

# `IO` y `Monoid`, `IO` y `Functor`

Este ejercicio consiste en definir funciones de tipo `IO algo`
en una sola línea solo usando funciones de las clases `Monoid` y `Functor`.

Descargar y resolver [MonFunIO.hs](MonFunIO.hs).

# `Functor`

En un módulo `Functor.hs`:

1. Implementar una instancia de `Functor` para `Either`. ¿Será
   exactamente `Either` el tipo que puede ser instancia de
   `Functor`? Pensar en la aridad (kind).

2. Definir estos dos tipos, e implementar sus instancias de `Functor`

~~~haskell
data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))
~~~



# Mónada `Maybe`

![](monads.jpg)

Escribí una función que detecta si una String tiene cierto formato.
El formato requerido es el siguiente:

1. La string empieza con un dígito.
2. Llamemos `n` el valor de ese dígito. La string contiene luego `n`
   caracteres `'a'`.
3. Después de las `'a'`, o la string se termina, o la secuencia se repite,
   empezando con un dígito (puede ser distinto).

Ejemplos:

Buenas strings     Malas strings
--------------     -------------
3aaa2aa            3aaa2a
9aaaaaaaaa         10aaaaaaaaaa
0                  1
001a               100a
2aa2aa             2bb2bb

Tu función debe usar la mónada `Maybe`. Debería parecerse a esto:

~~~haskell
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
  where go :: String -> Maybe String
        -- go evalua a Just "" si es exitosa, sino a Nothing
        ...
~~~

Ayuda: usá `readMaybe :: Read a => String -> Maybe a` de
`Text.Read` y `stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]`
de `Data.List`.

# Monada `IO` y notación `do`: Top 10 de las palabras de un archivo

Algunas referencias útiles para resover este ejercicio:

  * capítulo ["Entrada y Salida"](https://cs.famaf.unc.edu.ar/~hoffmann/aprendehaskell/content/EntradaSalida.html)
    (hasta Aleatoriedad).
  * capítulo ["Módulos"](https://cs.famaf.unc.edu.ar/~hoffmann/aprendehaskell/content/Modulos.html),
    en particular `Data.List` y `Data.Map.Strict`
  * el buscador de funciones Hoogle: <https://www.haskell.org/hoogle/>

 1. Hacé un programa que lee un archivo de texto, e imprime su contenido en la salida estándar.

 2. Modificalo para que haga lo mismo pero pasando todos los carácteres
    a mayúsculas (usando la función `toUpper` del módulo `Data.Char`.

 3. Vamos a hacer un programa que hace un poco más de procesamiento de datos.

    La idea es hacer un programa que lee un archivo de texto y muestre las 10
    palabras más frecuentes.

    Como archivos de texto pueden bajar libros desde
    proyecto Gutenberg (<https://www.gutenberg.org/>) en formato `.txt`.
    Por ejemplo [Guerra y Paz de Leo Tolstoy](https://www.gutenberg.org/files/2600/2600-0.txt)
    tiene suficiente contenido para que tengamos resultados interesantes.

    ![](warandpeace.jpg)

    Se puede dividir el trabajo en 3 partes:

    1. leer el contenido del archivo como una lista de palabras
    2. contar cuantas veces aparecen cada palabra
    3. ordenar las palabras de más frecuente a menos frecuente, y mostrar
       las 10 más frecuentes

    Para implementar el paso 2, vamos a usar la estructura de datos 
    `Map` (diccionario) de `Data.Map.Strict`, porque es una estructura optimizada
    que puede soportar grandes cantidades de datos. Más precisamente, la idea es guardar las palabras
    y su número de ocurrencias en un diccionario de tipo `Map String Int`.

    Pasos:

    a. Importá los módulos `Data.Map.Strict` y `Data.List` dándoles un prefijo
       para cada uno para evitar colisiones de nombres (ver "import qualified"
       en Aprende Haskell). Definí una función `main` que lee el contenido
       de un "input.txt"
       y define las palabras como una lista de Strings.
    b. Definí una función `agregar :: Map String Int -> String -> Map String Int`
       que agrega una palabra a un diccionario (posiblemente el diccionario
       ya tiene la palabra!).  Fijate (en Hoogle)
       en la documentación de `Data.Map.Strict` qué función(es) te puede(n) ayudar para definir `agregar`. 
    c. Modificá el `main` para agregar a un diccionario vacío (buscar en
       `Data.Map.Strict`) todas las palabras del archivo (¿qué función de orden superior
       permite hacer eso?).
    d. Definí la función `top10 :: Map String Int -> [(Int, String)]`
       tal que `top 10 m` sean las 10 palabras más frecuentes del
       diccionario `m`.  Definir `top10` requiere (entre otras cosas) ordenar una lista.
       Fijate en el módulo Data.List qué función(es) puede(n) ser útil(es) para eso.
    e. Imprimí el top 10 de las palabras del archivo en la salida estándar.

   Después de terminar cada paso podés mostrar en la salida estándar lo que
   llegaste a definir, por ejemplo usando la función `show` en combinación
   con `putStrLn`.

   Podés mejorar el programa implementando tu propia función
   que corta una `String` en lista de palabras, tomando en cuenta la puntuación,
   para mejorar la precisión del programa. Por ejemplo, no te sirve que tu programa
   cree que "Hola," (con la coma) es una palabra; tampoco te sirve que "Hola" y "hola"
   sean consideradas como dos palabras distintas.


