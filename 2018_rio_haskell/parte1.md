---
title: El sistema de tipos de Haskell | 1/5
author: Guillaume Hoffmann
...

# Presentaciones

## Su humilde servidor

  * Guillaume Hoffmann `[guióm]` <guillaume.hoffmann@conicet.gov.ar>
  * Cordobés desde el 2011
  * Investigador CONICET en la UNC
    * lógica matemática
  * Profesor en la Universidad Blas Pascal
    * introducción a la programación (C)
    * programación declarativa (Haskell)
  * Mantenedor de Darcs (Haskell)

# ¿Porqué este curso?

## Porqué Haskell

* el lenguaje funcional más enseñado en Argentina
* tiene cierta masa critica mundialmente
* sistema de tipos relativamente preciso
* está vivo / pasan cosas interesantes
* hay empleo

## El temido piramide de Haskell

~~~
^    más complejo
|   
|         / \          <- lo que se publica en internet
|        /   \
|       /     \ 
|      /       \       <- poco material de eso
|     /         \ 
|    /           \
|   /             \    <- lo que hay que saber para empezar
|   ---------------
|   
|    menos complejo
~~~

. . .

* modificar software existente es complicado
* errores de tipos misteriosos
* el objetivo es ser más proficiente en modificar software Haskell

## Ustedes

¿Quiénes son? :)

Este curso supone que ya saben:

* recursividad
* estructuras de datos funcionales
* map, filter,  foldr

Y no cubre:

* laziness
* performancia

## Contenido / Plan

 1. Lunes: tipos, clases de tipos, Monoid
 2. Martes: IO, Functor, Applicative, Monad, newtype
 3. Miércoles: multiclases, functional dependencies, type families
 4. Jueves: GADTs, tipos fantasmas, tipos existenciales
 5. Viernes: RankNTypes, DataKinds, TypeInType

## Para qué necesitamos a los sistemas de tipos

> Los sistemas de tipos sirven para hacer programas
> correctos e impedir que se puedan representar
> estados incorrectos.
>
> Julie Moroniki - *Haskell Programming from
> first principles*

. . .

> "A type system is a **tractable** syntactic method of
> **proving the absence** of certain program behaviors
> by classifying phrases according to the kinds of
> values they compute.”
>
> Benjamin Pierce - *Types and Programming Languages*

. . .

* Ayuda a clarificar el pensamiento y expresar la estructura de un programa
* Es una forma de documentación
* Convierte errores de ejecución en errores de compilación

# Día 1

## Temas

* uso de GHCi
* tipos algebraícos en Haskell estándar
* polimorfismo
* inferencia de tipos
* clases de tipos
* kinds
* instancias huérfanas
* newtype

## Preparándonos

* tengamos GHC 8.0 o más
* GHC 8.2 tiene mensajes de errores más lindos
* bajar desde <http://www.haskell.org/ghc> 

## Machete GHCi

`ghci` es el intérprete, `ghc` el compilador.

* `ghci archivo.hs`

* editar archivo corriente `:e`

* listar funciones del módulo corriente : `:browse`

* pedir tipo de una expresión: `:t expresion`

* pedir más datos sobre expresión/tipo/clase: `:info bla`

* elegir un editor potable: `:set editor vi`

* salir: `CTRL+D`

## Uso básico de GHCi

Evaluar expresiones y mostrar su valor:

~~~haskell
> 10 + 12
22

> "Hola" ++ " mundo"
"Hola mundo"

> words "Hola mundo"
["Hola","Mundo"]

> map show "abcdefg"
["'a'","'b'","'c'","'d'","'e'","'f'","'g'"]
~~~

## Anotaciones de tipo

~~~haskell
> :t "Hola"
"Hola" :: [Char]

> :t []
[] :: [a]

> :t [] ++ []
[] ++ [] :: [a]

> :t 10
10 :: Num p => p
~~~

. . .

Se pueden poner en código Haskell para precisar algun tipo:

~~~haskell
> :t [] :: [Int]
[] :: [Int] :: [Int]

> :t 10 :: Integer
10 :: Integer :: Integer
~~~

## Huecos tipados

* La expresión `_` (guión bajo) es un *hueco*
* genera un error cuando se evalua
* GHC(i) nos dice su tipo en el mensaje de error

. . .

~~~haskell
> [_] :: [Int]

<interactive>:10:2: error:
    • Found hole: _ :: Int
    • In the expression: _
      In the expression: [_] :: [Int]
      In an equation for ‘it’: it = [_] :: [Int]
    • Relevant bindings include
        it :: [Int] (bound at <interactive>:10:1)
~~~

##

Probar:

    > _ :: Int
    > _ :: Maybe Char
    > _ :: Int Char
    > _ :: Maybe Maybe

## Inferencia de tipos

Haskell tiene *inferencia de tipos*.

No hace falta indicar todos los tipos de las funciones
porque los tipos conocidos se *propagan* al resto del código.

El mecanismo de esa propagación es la *unificación*.

## Unificación

Algoritmo nacido en lógica (usado en lógica de primer
orden por ejemplo).

Dados dos términos conteniendo alguna variable, encontrar,
si existe, la sustitución más simple (es decir, una
asignación de algún termino a cada variable) que hace
iguales a los dos términos. La sustitución resultante
se llama unificador *más general* (*most general unifier*).

. . .

Terminos unificables:

* a, b
* a , Char
* [a] , [Int]
* Maybe a , Maybe String
* Either String a , Either b Int

. . .

Terminos no unificables:

* Char, Int
* Maybe Int, [Int]

## Errores de tipeo

~~~haskell
Prelude> (1 :: Int) +  (10 :: Integer)

<interactive>:3:16: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Integer’
    • In the second argument of ‘(+)’, namely ‘(10 :: Integer)’
      In the expression: (1 :: Int) + (10 :: Integer)
      In an equation for ‘it’: it = (1 :: Int) + (10 :: Integer)
~~~

. . .

Cuando falla la unificación, tenemos un error de tipeo.
GHC nos informa la discrepancia entre tipo esperado y tipo encontrado.

Esos mensages pueden ser más complejos que en un lenguaje sin inferencia de
tipos.

Porque el lugar donde la unificación falla no es siempre donde intuitivamente
está el error.

## Ejemplos de inferencia de tipos con unificacion

~~~haskell
map f [] = []
map f (first:rest) = f first : map f rest
~~~

* Tipo de map: `a -> b -> c`
* `b = [d]`
* `f :: d -> e`
* `c = [e]`

## Tipos de datos algebraicos (palabra-clave `data`)

  * Si ningun constructor tiene parametros, se llama *tipo enumerado*:

    ~~~haskell
    data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado
    ~~~

  * Si tiene un solo constructor, se llama *tipo producto*

    ~~~haskell
    data Coord = Coord Int Int
    ~~~

  * Un tipo con múltiples constructores se llama *tipo suma*

    ~~~haskell
    data Either a b = Left a | Right b
    ~~~

  * Un tipo sin constructores se llama *tipo vacío* (a veces, sirve):

    ~~~haskell
    data Vacio
    ~~~

## Sobrecarga de funciones

En programación, suele pasar que queremos sobrecarga de
algunas funciones = un mismo nombre de función con
distintas implementaciones según el contexto:

* `+`
* `==`
* `>=`

¿Cómo integrar bien la sobrecarga en un lenguaje como Haskell?

. . .

Solucion en Haskell:

> *la sobrecarga se debería reflejar en el tipo de una función*

Y así nacieron las clases de tipos.

## Repaso de clases de tipos conocidas

Ejecutar en GHCi:

    > :info Eq
    > :info Ord
    > :info Num

Observamos:

   * `MINIMAL`
   * contextos en definición de clases
   * contextos en instancias



## Ejemplos de clases de tipos

~~~haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
~~~

. . .

Función usando sobrecarga:

~~~
member :: Eq a => a -> [a] -> Bool
member x []                 = False
member x (y:ys) | x==y      = True
                | otherwise = member x ys
~~~

## Definiendo instancias

~~~haskell
data Coord = Coord Int Int

instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
  c1 /= c2 = not (c1 == c2)
~~~

. . .

Instancia para tipo polimórfico:

~~~haskell
instance (Eq a) => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = (x == y) && (xs == ys)
  xs     /= ys     = not (xs == ys)

~~~

## Ejercicio

¿Los polinomios pueden ser números?

Veamos el práctico.

## La clase `Monoid`

~~~haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty, mappend #-}
~~~


. . .

Las instancias de Monoid deben seguir ciertas leyes:

~~~haskell
mappend mempty x = x

mappend x mempty = x

mappend x (mappend y z) = mappend (mappend x y) z

mconcat = foldr mappend mempty
~~~

## Instancias y modulos Haskell

En Haskell, las instancias no tienen nombre.

Cuando se importa un módulo M, se importan todas las instancias definidas adentro,
no se puede elegir cuáles:

* no se pueden importar explicitamente
* no se pueden excluir explicitamente
* todas las instqancias de M se importan cuando se importa M o cuando
  se importa algun modulo que importa M (directamente o indirectamente)

## Instancias huerfanas

hay 3 lugares posibles para definir una instancia C T:

* en el módulo donde se define C
* en el módulo donde se define T
* en otro módulo (pero que importe los módulos donde se definen C y T)

El último caso es el caso de una instancia huérfana. GHC emite un warning
cuando encuentra una.

## Porqué hay que evitar las Instancias Huerfanas

Porque puede haber colisiones de instancias para un mismo par C y T.

Recordar que no se pueden excluir instancias cuando se importa un módulo.

Si se esta diseñando una libreria, esta mal visto tener instancias huerfanas.

Atenta a la modularidad de las librerias.

Si es una aplicacion, cierta gente las tolera.

Otros lenguajes las prohiben directamente (PureScript).

## Para saber mas sobre ese debate

* <http://wiki.haskell.org/Multiple_instances>
* <http://wiki.haskell.org/Orphan_Instances>
* <http://stackoverflow.com/questions/3079537>  "Orphaned instances in Haskell" (2010)

Porqué permitir importar explicitamente puede causar problemas:

<https://stackoverflow.com/questions/8728596/explicitly-import-instances/8731340#8731340>
 
## Conclusiones

Para mañana:

* completar el práctico
* si hace falta, ejercicios en Mumuki (ver página de la materia)

