% Práctico Haskell 3: MultiParamTypeClasses, FunctionalDependencies, TypeFamilies

# La multi-clase `Collection`

Hacer este ejercicio en un archivo `Multi.hs`.
Será muy probable necesario agregar al principio del archivo
unas pragmas para artivar extensiones del lenguaje.
Esas pragmas tienen la forma:

~~~haskell
{-# LANGUAGE NombreExtension #-}
~~~

Consideremos la multi-clase `Collection` (no contiene la función `empty` a propósito):

~~~haskell
class Eq e => Collection c e where
    insert :: c -> e -> c
    member :: c -> e -> Bool

instance Eq a => Collection [a] a where
    insert xs x = x:xs
    member = flip elem
~~~

Agregar una instancia `Collection (Set a) a`.

~~~haskell
import qualified Data.Set as S
~~~

Documentación del módulo: <https://hackage.haskell.org/package/containers>

Probar en GHCi lo siguiente:

~~~haskell
esFalso = member ( insert (S.empty :: S.Set Int) (1 :: Int) ) ( 2:: Int)
~~~

Como la clase no provee `empty` ponemos la constante `S.empty`.


# ¡Diversión con las dependencias funcionales!

En este ejercicio usamos el artículo "Fun with Functional Dependencies"
de Thomas Hallgreen:

<http://www.cse.chalmers.se/~hallgren/Papers/wm01.html>

La idea es adaptar el código del artículo en un módulo que pueda ser usado
por GHCi. A continuación algunas preguntas y observaciones para hacer la lectura más
progresiva.

## Secciones 1 hasta 3.2.1 incluido

¿Porqué es necesario incluir las funciones `isEven` e `isOdd` en las clases
`Even` y `Odd`?

## Sección 3.2.2

Dibujá los tipos y clases de tipos de esta sección
al estilo diagrama de Venn..

## Sección 3.2.3

Por primera vez vemos clases que definen relaciones entre 3 tipos.
La anotación `a b -> c` significa que el tipo de `a` y de `b`
(en este orden) implica el tipo de `c`.

## Sección 3.3

Podés saltar esta sección, lo más interesante viene después.

## Sección 3.4

Esta sección es tal vez la más interesante, pero también la más
difícil. Si viste algo de Prolog, el ejemplo te va a traer recuerdos.

Cuidado que esta sección contiene (¿a propósito?) errores en el
código, deberás pensar un poco para arreglarlos.
Además no es necesario dar constructores para los tipos `Nil`
y `Cons`.

Para comprobar las definiciones, usar los tests siguientes:

~~~haskell
> :t downfrom (u :: Three)
> :t insert (u :: Succ Zero) (u :: Cons Zero Nil)
> :t sort (downfrom (u :: Three))
~~~

# Más diversion con las dependencias funcionales

En el mismo estilo que el ejercicio anterior, está el artículo
["Type-level Instant Insanity"](https://wiki.haskell.org/wikiupload/d/dd/TMR-Issue8.pdf),
un poco más reciente (del 2007).
