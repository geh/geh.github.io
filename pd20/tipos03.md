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

## La clase `Applicative`

`Applicative` es una clase que está "entre" `Functor` y `Monad`.

~~~haskell
class (Functor f) => Applicative f where  
  pure :: a -> f a  
  (<*>) :: f (a -> b) -> f a -> f b  
~~~

* `pure` es exactamente el `return` de `Monad`.
* `<*>` generaliza `fmap` pero no es tan poderoso cono `>>=`.

Se inventó la clase `Applicative` después de `Monad`.

A veces la potencia de `Monad` es demasiado, y `Applicative` basta
(algunos parsers son más elegantes con `Applicative` que con `Monad`).

# Más sobre clases de tipos

* Typeclasseopedia: <https://wiki.haskell.org/Typeclassopedia>
  referencia de las clases de tipos más importantes de Haskell
  con sus leyes
