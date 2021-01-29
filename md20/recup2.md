% MD3 -- Recuperatorio 2

Los ejercicios se deben resolver sobre papel y ser mandados
a <guillaumh@gmail.com>.

La duración del examen es 1 hora.

# 1. Mostrar que un lenguaje es decidible (5 puntos)

Mostrar que el lenguaje binario siguiente es decidible:

`L = { w | |w| > 0 y la cantidad de 1s es igual a la cantidad de 0s en w }`

Por ejemplo las cadenas siguientes pertenecen a $L$:

* $10$
* $0101$
* $110100$

Las siguientes no pertencen a $L$:

* $100$
* $111$
* $1110100$

Describí una máquina de Turing de 3 cintas que decida este lenguaje.

La primera cinta es la cinta de entrada, la segunda una cinta de trabajo para almacenar la cantidad
de 0s leidos, la tercera para almacenar la cantidad de 1s leidos.

Los cabezales pueden usar los movimientos `<-`, `->` y `=` (permanecer inmóvil).

1. Escribí la tabla de transiciones de la máquina. No pongas una columna
   para cada combinación de 3 símbolos leidos, ya que muchas combinaciones son imposibles.
   Pone los títulos de columnas en el formato `(a,b,c)` donde `a` es el símbolo leido en la cinta 1,
   `b` el de la cinta 2, `c` el de la cinta 3.   Debes indicar los movimientos de los 3 cabezales siempre,
   pero podés omitir el símbolo de escritura si se deja el mismo.
2. Descibí textualmente qué étapa del cálculo hace cada estado. 

# 2. Mostrar que un lenguaje pertenece a **NP** (5 puntos)

Este es un lenguaje adaptado del problema "longest increasing subsequence":

`LIS = { (k, L) | k > 0  y L es una secuencia de enteros, que tiene una subsecuencia creciente de tamaño k }`

Por ejemplo, `<(6, {0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15})>` pertenece a `LIS`
dado que la secuencia tiene (por lo menos) la subsecuencia de tamaño 6 siguiente:
`{0, 2, 6, 9, 11, 15.}`.

En cambio, `(3, {2, 5, 4, 1})` no pertenece a `LIS`.

Explicar por qué `LIS` pertenece a **NP**.


