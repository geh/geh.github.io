% Teórico: el sistema de tipos de Haskell

# El sistema de tipos de Haskell, clases de tipos

## Para qué necesitamos a los sistemas de tipos

> Los sistemas de tipos sirven para hacer programas
> correctos e impedir que se puedan representar
> estados incorrectos.
>
> Julie Moroniki - *Haskell Programming from
> first principles*

> Un sistema de tipos es un método sintáctico
> **eficiente** para **demostrar la ausencia** de
> ciertos comportamientos del software, clasificando
> oraciones según el tipo de valores que calculan.
>
> Benjamin Pierce - *Types and Programming Languages*

. . .

* Ayuda a clarificar el pensamiento y expresar la estructura de un programa
* Es una forma de documentación
* Convierte errores de ejecución en errores de compilación

## Inferencia de tipos

Haskell tiene *inferencia de tipos*.

No hace falta indicar todos los tipos de las funciones
porque los tipos conocidos se *propagan* al resto del código.

El mecanismo de esa propagación es la *unificación*.

## Unificación

Dados dos términos conteniendo algunas variables,
encontrar, si existe, la sustitución más simple (es decir,
una asignación de algún termino a cada variable) que hace
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
* Maybe a, [b]

## Errores de tipeo

Cuando falla la unificación, tenemos un error de tipeo.
GHC nos informa la discrepancia entre tipo esperado y tipo encontrado.

~~~haskell
Prelude> (1 :: Int) +  (10 :: Integer)

<interactive>:3:16: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Integer’
    • In the second argument of ‘(+)’, namely ‘(10 :: Integer)’
      In the expression: (1 :: Int) + (10 :: Integer)
      In an equation for ‘it’: it = (1 :: Int) + (10 :: Integer)
~~~

## Sobrecarga de funciones

En programación, suele pasar que queremos usar un mismo nombre
de función pero que se refiera a distintas implementaciones
según el contexto:

* `+`
* `==`
* `>=`

En muchos lenguajes de programación, la sobrecarga de funciones
se hace "silenciosamente".

En Haskell, la sobrecarga se refleja en el tipo de las funciones.
Por esa razón existen las clases de tipos.

## Clases de tipos que conocemos.

Ejecutar en GHCi:

    > :info Eq
    > :info Ord
    > :info Num

Observamos:

   * `MINIMAL`
   * contextos en definición de clases
   * contextos en instancias

## Ejemplo de función usando sobrecarga

~~~haskell
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
~~~

Instancia para tipo polimórfico:

~~~haskell
instance (Eq a) => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = (x == y) && (xs == ys)
~~~

# Clases `Semigroup` y `Monoid`

## La clase `Semigroup`


Representa lo tipos que tienen una operación binaria asociativa.

Provista por `Data.Semigroup`:

~~~haskell
class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: GHC.Base.NonEmpty a -> a
  stimes :: Integral b => b -> a -> a
  {-# MINIMAL (<>) #-}
~~~

Implementación mínima:

~~~haskell
class Semigroup a where
  (<>) :: a -> a -> a
~~~

Las implementaciones deberían cumplir con
la propiedad de asociativiad:

~~~haskell
x <> (y <> z) == (x <> y) <> z
~~~

Ejemplo

~~~haskell
>>> [1,2,3] <> [4,5,6]
[1,2,3,4,5,6]
~~~

## La clase `Monoid`

Provista en `Data.Monoid`, es una sub-clase
de `Semigroup`:

~~~haskell
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty #-}
~~~

Implementación mínima:

~~~haskell
class Semigroup a => Monoid a where
  mempty :: a
~~~

`mappend` es definida por defecto como `<>` y sera removida de futuras versiones de GHC.

Las instancias deben cumplir con:

~~~
Neutro por la derecha
    x <> mempty == x
Neutro por la izquierda
    mempty <> x == x
Concatenación
    mconcat == foldr (<>) mempty
~~~

Para el tipo lista, `mempty` es la lista vacía.

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
 
## Volviendo a `Semigroup` y `Monoid`

~~~haskell
instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

-- estas instancias no son estándar:
instance Semigroup Bool where
  (<>) = (||) 

instance Monoid Bool where
  mempty = False
~~~

~~~haskell
> :m Data.Monoid
> "Hey" <> " " <> "you"
> False <> True <> False
~~~

## Instancias multiples, a propósito

¿Que pasa si queremos a proposito definir mas de una instancia
de algun tipo `T` en la clase `C`? Por ejemplo:

* `Monoid Int` con la operación suma y neutro 0
* `Monoid Int` con la operación producto y neutro 1

## `newtype`

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

A continuación un ejemplo de dos instancias solapadas de la clase `Show`:

~~~haskell
instance Show a => Show (Maybe a) where
    show _ = "chau"

instance Show (Maybe Int) where
    show _ = "hola"
~~~

Suponiendo que GHC acepte estas instancias en un mismo módulo, ¿qué pasa si se evalua la expresión
`show x` donde `x` es de tipo `Maybe Int`? ¿Veremos "hola" o "chau"?

En teoría tenemos una ambiguedad, pero podríamos aplicar la regla de que si hay una instancia más específica
que la otra, se elija esa. Para permitir instancias solapadas, y que se elija la instancia más específica,
se debe escribir justo después `instance`:

* `{-# OVERLAPPING #-}` en la instancia más específica
* `{-# OVERLAPPABLE #-}` en la instancia más general

En realidad, uno de los dos es suficiente:

~~~haskell
class C a where
 f :: a -> Int

instance C a => C [a] where
  ...

instance {-# OVERLAPPING  #-} C [Int] where
  ...
~~~

Ejercicio: resolver el ejercicio del práctico sobre las instancias solapadas.

## `IO` como miembro de la clase `Monoid`

La próxima semana vamos a ver el tipo `IO` más en detaller para escribir
programas que efectúan secuencias de acciones con entrada y salida: imprimir
algun mensaje, leer o escribir en archivos, solicitar un dato al usuario por
teclado, etc.

Cualquier función que haga una de esas acciones es de tipo `IO a`, donde
`a` es el tipo del resultado de la acción. En el caso que la acción no devuelva
resultado (como por ejemplo cuando se escribe en un archivo), su tipo es `IO ()`.

Si consultamos en `GHCi` con `:info Monoid`, vemos que existe una instancia
por defecto del tipo `IO` en la clase `Monoid` con la cabecera siguiente:

~~~haskell
instance Monoid a => Monoid (IO a)
~~~

Es decir, si algún tipo `a` pertenece a la clase `Monoid`, entonces
`IO a` tambien pertenece a `Monoid`.

¿Qué sería el neutro de `IO a`?

¿Qué sería la operacion binaria asociativa de tipo `IO a -> IO a -> IO a`?

El hecho que `IO` perteneza a `Monoid` nos provee una forma simple
de combinar acciones `IO` en secuencia, con la condición que todas
devuelvan el mismo tipo de valores, y que este tipo pertenezca
también a la clase `Monoid`.

Es una manera simple de combinar acciones `IO` aunque en la práctica
no se usa mucho.

# La clase `Functor`

## Motivación

Consideremos el patrón de "aplicar una función pura bajo un
constructor de tipo":

~~~haskell
mapList  :: (a -> b) -> List a  -> List b
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapIO    :: (a -> b) -> IO a    -> IO b
~~~

Podriamos crear una clase que contenga los tipos `List`, `Maybe` e `IO`
y que provea una función `map` genérica.

Esa clase es `Functor`.

## Definición y leyes

~~~haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
~~~

Leyes:

~~~haskell
-- Identidad
fmap id       ==  id
-- Composición
fmap (f . g)  ==  fmap f . fmap g
~~~

El significado de estas leyes es que si el tipo `f` representa
algun efecto (por ejemplo `IO`) o algun contenedor (como `Maybe`)
entonces aplicar una función via `fmap` no modifica ese efecto o
contenedor.

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

# Para explorar más

El [capítulo de Aprende Haskell sobre Monoides y Funtores](http://aprendehaskell.es/content/Funtores.html)
presenta esas clases de tipos de manera muy detallada, en un orden distinto de este apunte,
y además presenta la clase de tipo "Funtor Aplicativo", que es una subclase de "Funtor".
