% Examen final MDIII - Computabilidad y Autómatas

## 1. Lenguaje decidible (10 puntos)

Considerar el lenguaje siguiente:

$L=\{a^nb^ma^n\mid n,m>0\}$

Por ejemplo estas palabras pertenecen a $L$:  `aba`,  `abbba`,  `aaaabbaaaa`.

Describir una máquina de Turing que decida este lenguaje.

## 2. Clase de complejidad NP (10 puntos)

El "problema de la autopista con peajes" es el siguiente:
tenemos una lista de $n(n - 1) / 2$ enteros, con $n>1$.

Queremos saber si esta lista representa todas las distancias
entre cada par de puntos distintos, dentro de un conjunto de $n$
puntos colocados en una línea recta.
Puede ser que sí o que no.

Por ejemplo, esta lista : `{1,4,5,3,4,1}` sí representa 4 puntos colocados en
una línea recta. La información mínima para representar esta situación
es la lista `{1,3,1}`, que son las $n-1$ distancias sucesivas de estos
puntos en línea.

En cambio, la lista siguiente no tiene "solución" posible con 3 puntos en
una línea recta: `{10,1,100}`.

Llamamos $AP$ el lenguaje de las listas que tienen solución en el
problema de la autopista con peajes.

1. ¿Dado que no conocemos ningún algoritmo polinomial que decida $AP$,
   qué afirmación **no** podemos hacer?

2. Queremos explicar por qué $AP\in$ **NP**. ¿Qué sería el certificado $c$
   correspondiente a alguna instancia $l\in AP$?

3. ¿Qué sería el algoritmo que verifica que para cada par $(l,c)$, que $l\in AP$
   usando $c$ como certificado?

## 3. Lenguajes regulares (10 puntos).

Definir un autómata regular determinista que reconozca los lenguajes siguientes.
Estos lenguajes son definidos sobre el alfabeto $\{0, 1\}$:

1. El conjunto de todas las palabras que empiezan con `0` y terminan con `1`.
2. El conjunto de todas las palabras de la forma `01`, `0101`, `010101`, `01010101`, etc.

## 4. Conversión autómata a expresión regular (10 puntos)

Considerar el autómata siguiente:

~~~
        | 0 | 1 |
 -> * p | r | q |
      q | s | q |
      r | r | s |
      s | p | p |
~~~

1. Dibujá el grafo de este AFD.
2. Convertilo a una expresión regular equivalente, aplicando la
   técnica de eliminación de estados en el orden: s, r, q, p.

## 5. Conversión expresiones regulares a AFND-$\epsilon$ (10 puntos)

Dibujar el AFND-$\epsilon$ equivalente a la expresión regular dada:

1. $(10)^*1$
2. $((11^*) + 0)^*$


