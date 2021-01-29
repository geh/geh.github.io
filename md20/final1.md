% Examen final MDIII - Computabilidad y Autómatas
% 2020-12-09

## 1. Lenguaje decidible (15 puntos)

Considerar el lenguaje siguiente:

$L=\{0^n1^n0\mid n>0\}$

Por ejemplo estas palabras pertenecen a $L$:  `010`,  `00110`,  `0001110`.

Describir una máquina de Turing que decida este lenguaje, dando
su tabla de transición (no hace falta completar los casos que no pueden ocurrir),
y una explicación en castellano de qué función cumple cada estado de la máquina.

## 2. Clase de complejidad NP (10 puntos)

El "problema de la autopista con peajes" es el siguiente:
tenemos una lista de $n(n - 1) / 2$ enteros, con $n>1$.

Queremos saber si esta lista representa todas las distancias
entre cada par de puntos distintos, dentro de un conjunto de $n$
puntos colocados en una línea recta.
Puede ser que sí o que no.

Por ejemplo, esta lista : `{1,4,5,3,4,1}` sí representa 4 puntos colocados en
una línea recta de la forma siguiente:

~~~
----*-*---*-*-------
~~~

Observamos que la información mínima para representar esta situación
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


# 3. Conversión de AFN a AFD (15 puntos)


Dibujá el grafo de transiciones del siguiente
Autómata Finito Nodeterminista, y convertilo en
un Autómata Finito Determinista equivalente:

```
       |  0  |  1  |
-------+-----+-----+
->  p  |{q,s}| {q} |
   *q  | {r} |{q,r}|
    r  | {s} | {p} |
   *s  |  /  | {p} |
```

En la tabla que calculás para la conversión, solo
incluí los estados alcanzables.

# 4. Conversión de AFN-ε a AFD (10 puntos)


Dibujá el grafo de transiciones del siguiente
Autómata Finito Nodeterminista con transiciones ε,
y convertilo en un Autómata Finito Determinista equivalente:


```
         |    ε    |    a    |   b    |    c
-------------------------------------------------
-> p     |   {q}   |   {p}   |  {q}   |   /
   q     |   {r}   |   {q}   | {p,r}  |   /
   r     |   {p}   |    /    |   /    |  {s}
  *s     |    /    |  {s}    |   /    |   /
```


Antes de proceder a la conversión, hace la lista
de las clausuras ε de cada estado del AFN.

En la tabla que calculás para la conversión, solo
incluí los estados alcanzables.

