% Matemáticas Discretas 3 -- Parcial 3 (Autómatas y Lenguajes Regulares)

Duración del parcial: 1 hora.

Entregar el parcial a <guillaumh@gmail.com>.

# Ejercicio 1 (2 puntos)

Describí AFDs que acepten los siguientes lenguajes con el alfabeto $\{0, 1\}$:

a. El conjunto de todas las cadenas que terminan en 10.
b. El conjunto de todas las cadenas con tres ceros consecutivos (no necesariamente al final).

En cada caso dibujá el grafo de transiciones.

# Ejercicio 2 (4 puntos)

Dibujá el grafo de transiciones del siguiente
AFN y convertilo en un AFD equivalente:

```
       |  0  |  1  |
-------+-----+-----+
->  p  |{q,s}| {q} |
   *q  | {r} |{q,r}|
    r  | {s} | {p} |
   *s  |  /  | {p} |
```

# Ejercicio 3 (4 puntos)


Dibujá el grafo de transiciones del siguiente
AFN-ε y convertilo en un AFD equivalente:

```
         |    ε    |    a    |   b    |    c
-------------------------------------------------
-> p     |   {q}   |   {p}   |  {q}   |   /
   q     |   {r}   |   {q}   | {p,r}  |   /
   r     |   {p}   |    /    |   /    |  {s}
  *s     |    /    |  {s}    |   /    |   /
```



<!--

```
         |    ε     |    0      |   1    |    2
-------------------------------------------------
-> p     |  {q,r}  |    /      |  {q}   |  {r}
   q     |    /    |   {p}     |  {r}   | {p,q}
  *r     |    /    |   /       |   /    |   /
```

-->


