---
title: "Lección 2: Cálculo Lambda"
date: 2020
author: Universidad Blas Pascal - G. Hoffmann
...

# Cálculo lambda

## Qué es 

  * El cálculo lambda, o *lambda calculus* (*λ-calculus*)
    * formalismo matemático desarrollado en los años 1930s
    * lo puede ver como un lenguaje de programación minimalista
    * creado por [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church)
  * No tiene números, caracteres o Booleanos, o tipos de datos otros
    que funciones
  * Sin embargo puede representar cualquier Máquina de Turing

## Elementos del cálculo lambda

Las expresiones o términos lambda, se escriben utilizando 3 elementos básicos.

3 elementos: **variables**, **funciones**, y **aplicaciones**.

| Nombre      | Sintaxis                          | Ejemplo   | Explicación                                   |
|-------------|-----------------------------------|-----------|-----------------------------------------------|
| Variable    | `<nombre>`                        | `x`       | una variable llamada "x"                      |
| Función     | `λ<parámetro>.<término>`          | `λx.x`    | una función con parámetro "x" y cuerpo "x"    |
| Aplicación  | `<término><término>`              | `(λx.x)a` | llamar la función "λx.x" con el parámetro "a" |

La función más simple es la identidad: `λx.x` que es equivalente a `f(x) = x`.

Ejemplos:

~~~
    x
    xy
    xzyx
    (xz)(yx)
    λx.x
    (λx.x)y
    (λz.(λx.y))
    (x(λz.z))z
~~~

## Parámetros múltiples

Parece que el cálculo lambda solo soporta funciones con un solo
parámetro, pero podemos crear funciones que toman 2 parámetros o más con una
técnica llamada [currificación](https://en.wikipedia.org/wiki/Currying).

- `(λx.λy.λz.xyz)` equivale a `f(x, y, z) = ((x y) z)`

A menudo se escribe directamente `λxy.<cuerpo>` en lugar de: `λx.λy.<cuerpo>`

## Orden de aplicación

¿Qué significa el término `xyz`?  ¿Es (xy)z o x(yz)?

Respuesta : `(xy)z`

La aplicación es asociativa hacia la izquierda.

## Variables libres y variables ligadas

- En la función `λx.x`, "x" es una variable *ligada* porque está al mismo
  tiempo en el cuerpo de la función y es parámetro de ella.
- En `λx.y`, "y" es una variable *libre* porque no fue declarada en esa función.

## Cómo se "ejecuta" un término lambda?

Un término lambda se evalua con una regla llamada
[β-reducción](https://en.wikipedia.org/wiki/Lambda_calculus#Beta_reduction),
que es básicamente sustitución sintáctica.

Cuando se evalua la expresión `(λx.x)a`, se *reemplazan* todas las ocurrencias
ligadas de "x" en el cuerpo de la función por "a".

- `(λx.x)a` se vuelve: `a`
- `(λx.y)a` se vuelve: `y`

Incluso se puede crear funciones que devuelven funciones:

- `(λx.(λy.x))a` se vuelve: `λy.a`

## Problemas con reemplazo ingenuo

1. En general no queremos reemplazar *todas* las ocurrencias de "x", por ejemplo:

```
(λx.(x + ((λx.x+1)3)))2
```

Tenemos dos abstracciones anidadas que usan la misma variable como parámetro formal, "x".
Si reemplazamos *todas* las "x" por 2, cambiamos el sentido de la abstracción interna:

```
2 + ((λx.2+1)3))
```

Solución al problema 1: cuando queremos reducir `(λx.M)N`, debemos solo
reemplazar las ocurrencias *libres* en M por N.

## Problemas

2. Consideremos:

```
((λx.λy.x)y)z
``` 

Cuando está aplicada a 2 parámetros, la expresión `λx.λy.x` debería devolver el primero,
entonces la expresión anterior debería devolver `y`.

Pero si hacemos un reemplazo "ingenuo":

```
((λx.λy.x)y)z
(λy.y)z         <- `y` se convierte en variable ligada - cambia el significado de la abstacción
z
```

Solución: α-conversión = renombrar parametros formales para conservar el significado
de las abstracciones, antes de proceder a la reducción:

```
((λx.λy.x)y)z
((λx.λu.x)y)z   <- α-conversión
(λu.y)z
y
```

## La β-reducción no siempre reduce

El nombre es un poco engañoso porque indica que "reducir" un término lo achica.
En realidad, puede reducir, aumentar o dejar igual el tamaño de un término lambda.

Unos ejemplos:

```
(λx.xx)(λx.xx) 
(λx.xx)(λx.xx) 
```

```
(λx.xxx)(λx.xxx)
(λx.xxx)(λx.xxx)(λx.xxx)
(λx.xxx)(λx.xxx)(λx.xxx)(λx.xxx) 
```

```
(λx.xx)(λa.λb.bbb)
(λa.λb.bbb)(λa.λb.bbb)
λb.bbb
```

## Forma normal

Un término lambda está en *forma normal* si no se puede aplicarle más beta-reducción.

Un término lambda *tiene* forma normal si, aplicándole *alguna* secuencia de beta-reducciones,
se puede convertir en un término en forma normal.

## Preguntas interesantes

>  * Todos los términos lambda tienen forma normal?
>  * No. Por ejemplo `(λz.zz)(λz.zz)` que es un "bucle infinito".
>  * Si un término lambda tiene forma normal, ¿todos los "caminos"
>    posibles de beta-reducción nos llevan a ella?
>  * No. Por ejemplo `(λx.λy.y)((λz.zz)(λz.zz))`
>  * Si se aplica beta-reducción siempre en el sub-termino de la derecha, nunca termina.
>  * Si se aplica al de la izquierda, termina en un paso.
>  * ¿Existe una estrategia para siempre encontrar el camino que lleva a la forma normal, si existe?
>  * Sí. Se llama *leftmost-outermost* o *normal-order-reduction* (NOR), y vamos a verlo.

## Reducción normal

Para hacer una reducción "normal", siempre elegir, entre las *aplicaciones* posibles, las que están más
"afuera", y entre ellas, las que están más a la izquierda.

En programación, la reducción normal es parecida al estilo "call-by-name" para llamar funciones,
donde se evalua el parametro real recién cuando el parametro formal se necesita.

Si el parametro formal no se usa, entonces se ahorra el trabajo de evaluar el parametro real.

## Call-by-name / call-by-value

Seguramente están acostumbrados a lenguajes con "call by value", como C:

~~~C
int mas_uno(int x, int y){
    return x+1;
}

main(){
    mas_uno(10, contar_palabras_de_wikipedia());
}
~~~

Este programa siempre va a ejecutar `contar_palabras_de_wikipedia()`
por más que no se use el resultado.

Si C fuera un lenguaje "call by name" no lo haría.

## ¿Por qué la reducción normal siempre nos lleva a la forma normal, si existe?

La aplicación más afuera y más a la izquierda *no puede ser parte de
otra aplicación*.

Por lo cual, reducirla equivale a ejecutar el cuerpo de la función,
en lugar de evaluar su parametro actual.

Si esa función ignora su parámetro, entonces esa reducción puede
"hacer desaparecer" otras aplicaciones.

Por esa razón intuitiva, la reducción normal nos lleva a la forma
normal si existe, por más que otras secuencias de reducciones no lo harían.

##  Ejercicios

1. Indicar, para cada variable, cuáles de sus ocurrencias son libres y cuáles ligadas,
   en las siguientes expresiones. Indicar a que abstracción está ligada cada ocurrencia no libre.

* `(λy.y(λx.x)z)`
* `(λy.x(λx.x)z)`
* `(λy.y(λy.y)yx)`


2. Encontrar los subtérminos de las siguientes expresiones:

* `(λy.y(λx.x)z)`
* `(λy.x(λx.x)z)`

3. Indicar cuáles de los siguientes pares de expresiones son
   α-equivalentes y cuáles no lo son. Justificar cada respuesta.

* `(λxyz.x(λy.yz)w)`,        `(λtuv.t(λz.zv))w`
* `(λxyz.x(λy.yz)w)`,        `(λxyw.x(λy.yw)z)`
* `(λxyz.x(λy.yz)w)`,        `(λxtz.x(λu.tz))w`
* `(λxyz.x(λy.yz)w)(λx.xy)`, `(λxyw.x(λy.yz)w)(λz.zy)`

4. Considerar los siguientes términos.
   De ser posible, β-reducirlos hasta obtener su forma normal.

* `(λx.λy.xy)(λy.yz)`
* `(λx.λy.xy)(λz.yz)z`
* `(λx.(λy.x)yλz.z)(λy.yz)`
* `(λf.(λx.f(xx))(λx.f(xx)))`
* `(λn.λm.n(λn.λxy.nx(xy))m)(λxy.xxy)(λxy.xy)`


