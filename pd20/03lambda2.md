---
title: "Lección 3: Más Cálculo Lambda"
date: 2020
author: Universidad Blas Pascal - G. Hoffmann
...

# Teórico cálculo lambda - jueves 27/8/2020

## Repasando

  * beta-reducción: aplicar funciones con sustitucion de los
    parametros formales por parametros reales
  * alfa-conversión: renombrar parametros formales, en particular para
    evitar que cambie el significado de funciones cuando se hace beta-reducción

Ejemplo:

```
(λcd.(λy.c(cd)((cd)y))) (λxy.xy)
(λcd.(λa.c(cd)((cd)a))) (λxy.xy)                 <- alfa-conversion
(λd.(λa.(λxy.xy)((λxy.xy)d)(((λxy.xy)d)a)))
```

## η-conversión

Supongamos que `M` es un término lambda.

La η-conversión (η = eta) consiste en reemplazar `(λx.Mx)` por `M`
cuando `x` no aparece libre en `M`.

¿Por qué es correcto hacer ese reemplazo?

Adelanto: el linter de Haskell a veces sugiere aplicar η-conversión
a definiciones de funciones.

## Codificar más tipos de datos en cálculo lambda

Para demostrar que el cálculo lambda es capaz de representar cualquier
cálculo, Alonzo Church definió codificaciones para:

* booleanos
* enteros
* listas 
* funciones para manejar estos tipos

Se conoce como "Church encoding".

Veamos una parte de esa codificación, más bien a modo de entrenamiento.

## Lógica Booleana

Veamos cómo Church representa "True" o "False" en cálculo lambda,
y con qué funciones se pueden manejar.

`T` es representado por: `λx.λy.x`

`F` es representado por: `λx.λy.y`

Desde ahi podemos definir las primeras funciones booleanas:

> * ¿Cómo definir la función NOT?
> * Tiene que ser una función que tome T y devuelva F, y vice versa.
> * NOT = `λb. b F T`
> * ¿Cómo definir la función IF?
> * Tiene que tomar un booleano `b`, y dos términos `x` y `y`, y
>   si `b` es T, devolver `x`, sino devolver `y`.
> * IF = `λbxy.b x y`
> * Utilizando `IF`, podemos definir más operadores Booleanos
> * `AND`:
> * `AND a b` es definido como: `λab.IF a b F`
> * `OR`:
> * `OR a b` es definido como: `λab.IF a T b`

## Números

Church definió también números en cálculo lambda.

La secuencia empieza así:

> * `0 = λf.λx.x`
> * `1 = λf.λx.f x`
> * `2 = λf.λx.f(f x)`
> * `3 = λf.λx.f(f(f x))`

La primera operación que definimos con números de Church, es la función sucesor
`S(n) = n + 1`.

Veamos:

> * `n   = λf.λx.f(f(...(f x))` (aplica `f` n veces)
> * `n+1 = λf.λx.f(f(f(...(f x))` (aplica `f` n+1 veces)
> * `S = λn.λf.λx.f((n f) x)`

Ejercicio: comprobar que `S 2 = 3`.

> * Usando S, podemos definir la suma:
> * `ADD a b = λab.(a S)b`

## Recursión

Gracias a IF, podemos controlar la ejecución de un programa.
Podemos hacer bucles simples usando números de Church.

Pero todavía no podemos hacer un bucle "while" infinito o
una función recursiva.

En un lenguaje de programación común podríamos definir factorial así:

`fact(n) = if n <= 1 then 1 else n*fact(n-1)`

¿Cómo hacerlo en cálculo lambda con funciones que no tienen nombre?

Debemos definir un término FACT que satisface lo siguiente (supongamos que
tenemos definidas todas las macros):

`FACT = λn. IF (LEQ n 1) 1 (MUL n (FACT (SUB n 1)))`

Pero `FACT` aparece en su propia definición!

## Auto-aplicación 

Como `FACT` no puede referirse a si mismo, usemos un truco.

Definamos una función `F` que agrega un parámetro extra `f`
para recibir alguna función similar a `FACT`:

`F = λf. λn. IF (LEQ n 1) 1 (MUL n (f (SUB n 1)))`

Qué le damos a esta función F? F misma? Pero no funcionaría porque
dicho parámetro espera un parámetro más. Entonces modificamos esta
definición para que llame a `f` con un parámetro extra `f`:

`FACT' = λf. λn. IF (LEQ n 1) 1 (MUL n ((f f)(SUB n 1)))`

Y definimos `FACT = FACT' FACT'`.

Ejercicio: comprobar que `FACT 4` da `24` (suponer que `MUL` y otros
operadores andan como deberían).

## Consideraciones teóricas

Teorema de Church-Rosser: cualquier orden de aplicación de las reducciones
nos lleva al mismo término.

Tambien se dice que las reglas de reducciones del cálculo lambda son
*confluyentes*.

```
 a--------------------->b
 |                      |
 |                      |
 |                      |
 |                      |
 |                      |
 |                      |
 |                      |
 v                      v
 c--------------------->d
```

## Para leer y ver

* [Lambda Calculus - Computerphile](https://www.youtube.com/watch?v=eis11j_iGMs)
  en inglés con subtítulos y la traducción automática anda muy bien
* [Y Combinator - Computerphile](https://youtu.be/9T8A89jgeTI?t=274)
* En castellano: <http://www.lcc.uma.es/~blas/apuntes/PDAv/lambdaC.pdf>
* [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)
* [Cornell CS 312 Recitation 26: The Lambda Calculus](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
* Fuente de este apunte <https://learnxinyminutes.com/docs/lambda-calculus/>

