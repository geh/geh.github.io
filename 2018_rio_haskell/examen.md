% Examen RIO 2018: El sistema de tipos de Haskell/GHC

Para cada pregunta, hay **UNA SOLA** respuesta correcta.
Son 32 preguntas.

 1. Que la función `map` sea polimórfica significa que...

    a. sus argumentos pueden tener tipos distintos
    b. anda con listas de cualquier tamaño
    c. tiene diferentes definiciones posibles

 2. Una función sobrecargada es una función que...

    a. usa muchos recursos
    b. tiene más de un parámetro
    c. tiene más de una implementación

 3. En Haskell, el algoritmo que sirve de base para
    la inferencia de tipo es...

    a. la minimización de autómatas
    b. la unificación de términos
    c. el ordenamiento de listas
    d. la factorización de enteros

 4. Cuál de los tipos siguientes es bien formado:

    a. `Num a -> Maybe a => a`
    b. `Num a => Maybe a -> a`
    c. `Num a -> Maybe a -> a`

 5. La función `(>>=)` es proveida por la clase

    a. `Functor`
    c. `Monad`
    d. `Monoid`

 6. La función `fmap` es proveida por la clase

    a. `Monoid`
    b. `Functor`
    d. `Monad`

 7. La función `mappend` es proveida por la clase

    a. `Functor`
    b. `Monad`
    c. `Monoid`

 8. Un tipo siempre puede ser habitado por valores.

    a. Verdadero
    b. Falso

 9. Se pueden elegir qué instancias importar cuando uno
    importa un módulo.

    a. Verdadero
    b. Falso

10. Una instancia que no se define ni en el módulo de definición
    de la clase ni del tipo, es una instancia...

    a. incompleta
    b. huérfana
    c. redundante
    d. incorrecta

11. Para pertenecer a la clase `Monad`, un tipo tiene que
    pertenecer a la clase `Monoid`.

    a. Verdadero
    b. Falso

12. Si ya está definida la instancia del tipo polimórfico `T1 a`
    para alguna clase `C` (con `instance C (T1 a)`),
    se puede definir una instancia para
    algun tipo más específico `T1 T2`:

    a. Verdadero
    b. Falso

13. Sin extensiones activadas, GHC soporta clases de tipos
    múltiples.

    a. Verdadero
    b. Falso

14. La palabra-clave `newtype` permite definir...

    a. un tipo sinónimo a algun tipo existente
    b. un tipo distinto isomorfo a algun tipo existente
    c. una clase de tipo nueva

15. Cuál de las clases siguientes define cómo combinar
    funciones de tipo `IO`:

    a. `Functor`
    b. `Monad`
    c. `Num`

16. La notación `do` permite evitar de escribir la función:

    a. `fmap`
    b. `<$>`
    c. `>>=`

17. Las leyes que deberían cumplir las instancias de clases
    como `Eq`, `Monoid`, `Monad`, etc. son chequeadas por el
    compilador.

    a. Verdadero
    b. Falso

18. Para una clase de tipos múltiples `class C a b`,
    y un tipo concreto `T`, solo está permitido tener una sola
    instancia de la forma `instance C T b`.

    a. Verdadero
    b. Falso

19. Las dependencias funcionales se pueden agregar a...

    a. la definición de alguna clase
    b. la definición de alguna instancia
    b. la definición de alguna función sobrecargada

20. Una familia de tipos (type family) es...

    a. una relación sobre tipos
    b. una relación sobre valores
    c. una función sobre tipos
    d. una función sobre valores

21. Dado el tipo `M a -> b`, `M` puede ser:

    a. solo un tipo contenedor
    b. solo una familia de tipos
    c. las dos cosas

22. Si `F` es una familia de tipos de kind `* -> *`, puedo escribir
    una función de la forma siguiente:

    a. f :: F a -> b
    b. f :: a -> b -> 
    c. F :: F F -> b -> c

23. En GHCi puedo forzar la evaluación de una familia de tipos usando
    el commando...

    a. `:type`
    b. `:type!`
    c. `:kind`
    d. `:kind!`

24. Sea `data P = forall x . P x`. El tipo `x` es un:

    a. tipo fantasma
    b. tipo existencial
    c. tipo dependiente

25. Sea `data Q x = Q Int`. El tipo `x` es un:

    a. tipo fantasma
    b. tipo existencial
    c. tipo dependiente

26. La extensión `GADTs` introduce una sintaxis nueva para

    a. definir clases de tipos
    b. definir tipos de datos
    c. definir instancias de clases

27. Sea `data T = A | B Bool | C Char`. `T` es un tipo:

    a. suma
    b. producto

28. Sea `data T = T Bool Bool Bool`. `T` es un tipo:

    a. suma
    b. producto

29. El kind de `Num` es:

    a. `*`
    b. `* -> Constraint`
    c. `* -> *`

30. Las extensiones `DataKinds` y `TypeInType` colapsan...

    a. los valores y los tipos
    b. los tipos y los kinds
    c. los valores y los kinds

31. Sea `data Nat = Zero | Suc Nat`. Si activamos `DataKinds`,
    `Nat` se vuelve también:

    a. un valor
    b. un kind
    c. los dos

32. ~~~haskell
    data Zim a where
      Zam    :: Int -> Zim Int
      Zum    :: Char -> Zim Char
    ~~~

    Dada esta definición, se puede construir algun valor de tipo
    `Zim Bool`.

    a. Verdadero
    b. Falso

