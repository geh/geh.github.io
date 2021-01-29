% La clase `Monad`

# 1. `Maybe` como mónada

Escribí una función que detecta si una `String` tiene cierto formato.
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
tieneBuenFormato :: String -> Bool
tieneBuenFormato = isJust . go
  where go :: String -> Maybe String
        -- go evalua a Just "" si es exitosa, sino a Nothing
        ...
~~~

Ayuda: usá `readMaybe :: Read a => String -> Maybe a` de
`Text.Read` y `stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]`
de `Data.List`.


# 2. Una mónada para construir parsers: parseando archivos `CSV`

Los combinadores de parsers son un buen ejemplo de qué tan prácticas son las mónadas.
En este ejercicio, vas a construir una pequeña
librería de combinadores de parsers. Al final, el código siguiente podrá,
por ejemplo, parsear archivos `CSV`:

~~~haskell
parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content
~~~


Tratá de entender el código anterior. El operador `<*` viene de la clase
`Applicative`, hace dos acciones en secuencia y descarta el valor obtenido
por su segundo parámetro.

1.  Definí el tipo `Parser`:

    ~~~haskell
    data Parser a = P (...)
    ~~~

    Un valor de tipo `Parser a` es una función que toma una String
    (que representa el resto de la entrada en un momento dado) y
    devuelve o `Nothing` si el parsing fracasa, o sino devuelve un
    valor de tipo `a` y el resto de la entrada.

    Definí también

    ~~~haskell
    runParser :: Parser a -> ...
    runParser (P p) = p
    ~~~

    para sacar el constructor `P`. Usá esto en lugar de hacer pattern matching
    explícito en las funciones más abajo, porque sino ciertas definiciones
    recursivas van a entrar en un bucle, porque el pattern matching (y así
    la evaluación) ocurre demasiado temprano.

2.  Definí la función

    ~~~haskell
    parse :: Parser a -> String -> Maybe a
    ~~~

    que es el punto de entrada principal para ejecutar un parser. Debe devolver
    un valor exitoso solo si el parser consumió toda la entrada, es decir,
    si la función dentro de  `Parser a` devuelve un valor de tipo `a` junto con
    una String vacía.

3.  Implementá una función

    ~~~haskell
    noParser :: Parser a
    ~~~

    que representa un parser que falla siempre.

    Deberías tener

    ~~~haskell
    parse noParser input == Nothing
    ~~~

    para cualquier entrada `input`, incluso la String vacía.

4.  Implementá una función

    ~~~haskell
    pureParser :: a -> Parser a
    ~~~

    que representa el parser que no consume ninguna entrada y devuelve
    su argumento.

    Deberías tener
    
    ~~~haskell
    parse (pureParser x) "" == Just x
    xs ≠ "" ⇒ parse (pureParser x) xs == Nothing
    ~~~

5.  Declará

    ~~~haskell
    instance Functor Parser where
        fmap ...
    ~~~

    Deberías tener

    ~~~haskell
    parse (fmap f p) input == fmap f (parse p input)
    ~~~

    para cualquier `f`, `p` e `input`.

6.  Declará

    ~~~haskell
    instance Applicative Parser where
        pure = pureParser
        fp <*> fx = ...
    ~~~

    que aplicá el parser de la izquierda a la entrada primera para
    conseguir la función. Si tiene éxito, aplicá el parser de la derecha
    al resto de la entrada para conseguir el argumento, y devuelve la
    función aplicada al argumento, y el resto de la entrada
    en el argumento correcto.
   
7.  Declará

    ~~~haskell
    instance Monad Parser where
        return = pureParser
        fa >>= k = …
    ~~~

    que funciona de forma bastante similar a `<*>`.

8.  Definí el parser primitivo

    ~~~haskell
    anyChar :: Parser Char
    ~~~

    que fracasa si la entrada es vacía, y sino saca un carácter de
    la entrada:

    ~~~haskell
    parse anyChar ""                 == Nothing
    parse anyChar "c"                == Just c
    length xs > 1 ⇒ parse anyChar xs == Nothing
    ~~~

9.  Definí las funciones

    ~~~haskell
    char :: Char -> Parser ()
    anyCharBut :: Char -> Parser Char
    ~~~

    **sin** romper la abstracción introducida por el tipo de datos `Parser`,
    es decir, solo usando combinadores introducidos más arriba. Podés
    usar la notación `do` si querés.

10. Definí el combinador

    ~~~haskell
    orElse :: Parser a -> Parser a -> Parser a
    ~~~

    que probá usar el parser de la izquierda. Si es exitoso, lo usa,
    sino ejecuta el segundo parser en su entrada. Esto implementa
    el backtracking de manera muy ingenua (entonces no te esperes
    a que esto tenga muy buena performancia -- existen librerias
    de parsers muy optimizadas en este aspecto).
    
    Deberías tener

    ~~~haskell
    parse (noParser `orElse` p) input == parse p input
    parse (pureParser x `orElse` p) input == parse (pureParser x) input
    parse (anyChar `orElse` pureParser '☃') "" == Just '☃'
    parse (anyChar `orElse` pureParser '☃') [c] == Just c
    length xs > 1 ⇒ parse (anyChar `orElse` pureParser '☃') xs == Nothing
    ~~~

11. Definí el combinador

    ~~~haskell
    many :: Parser a -> Parser [a]
    ~~~

    que aplica un parser dado tantas veces como se pueda hasta que falle,
    y luego devuelve el resultado como una lista.

    Implementalo usando las funciones `pure` y `<*>` de la clase `Applicative`.

    Deberías tener

    ~~~haskell
    parse (many anyChar) xs = Just xs
    parse (many noParser) "" = Just []
    not (null xs) ⇒ parse (many noParser) xs = Nothing

    -- if no '\n' in xs, then also:
    parse (many anyCharBut '\n' <* char '\n') (xs++"\n") = Just xs
    ~~~

    Algunos "fun facts" sobre `many` para que lo piensas (no son parte
    del práctico):

    -   El parser `many p` nunca falla.
    -   La expresión `many (pure x)` no es muy útil. ¿Ves por qué?
    -   ¿Qué pasa si aplicás `many` a `many`?

12. Definí el combinador

    ~~~haskell
    sepBy :: Parser a -> Parser () -> Parser [a]
    ~~~

    de tal forma que `p1 sepBy p2` aplica `p1`, luego `p2`, luego `p1` y así.
    Tiene éxito si la primera invocación de `p1` falla, en este caso devuelve
    la string vacía. También tiene éxito si cualquier invocación de `p2` falla,
    en cual caso devuelve los resultados de todas las invocaciones de `p1` como
    lista. De vuelta, implementalo *sin* romper la abstracción, usando los
    combinadores vistos más arriba.

    Deberías tener

    ~~~haskell
    -- si xs no es vacía y no termina con '\n', entonces
    parse (many (anyCharBut '\n') `sepBy` char '\n') xs = Just (lines xs)
    ~~~

Con todos esto armado, probá tu parser de `CSV`:

~~~haskell
parse parseCSV "\"ab\",\"cd\"\n\"\",\"de\"\n\n"
    == Just [["ab", "cd"],["","de"],[]]
~~~

# Fuente

<https://www.seas.upenn.edu/~cis194/fall16/hw/08-functor-applicative.html>
