% Matemáticas Discretas 3 -- Recuperatorio 3

Autómatas Finitos.

# Ejercicio 1 (25 puntos)

1. Dibujá el grafo de transiciones del siguiente AFN

```
       |  a  |  b  |
-------+-----+-----+
->  p  |{r,s}| {r} |
    q  | {s} | {p} |
   *r  | {q} |{q,r}|
   *s  |  /  | {p} |
```

2. Convertilo en un AFD equivalente (solo listá los estados accesibles en la tabla
   de transición).

3. Dibujá el grafo de transiciones del AFD generado.


# Ejercicio 2 (25 puntos)


1. Dibujá el grafo de transiciones del siguiente AFN-ε


```
         |    ε     |    0      |   1    |    2
-------------------------------------------------
-> p     |  {q,r}  |    /      |  {q}   |  {r}
   q     |    /    |   {p}     |  {r}   | {p,q}
  *r     |    /    |   /       |   /    |   /
```



<!--
```
         |    ε    |    a    |   b    |    c
-------------------------------------------------
-> p     |   {q}   |   {p}   |  {q}   |   /
   q     |   {r}   |   {q}   | {p,r}  |   /
   r     |   {p}   |    /    |   /    |  {s}
  *s     |    /    |  {s}    |   /    |   /
```
-->


2. Convertilo en un AFD equivalente (solo listá los estados accesibles en la tabla
   de transición).

3. Dibujá el grafo de transiciones del AFD generado.

