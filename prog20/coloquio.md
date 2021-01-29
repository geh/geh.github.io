% Programación 1: Coloquio

# 1. Arreglos (5 puntos)

Hacer este ejercicio en un archivo **arreglo.c**.

 1. Definir una constante `T` valiendo 5. Declarar un arreglo de `T` enteros. Pedir el ingreso de estos `T` valores al usuario
    y cargarlos en el arreglo.
 2. Calcular e imprimir la suma de los elementos del arreglo que sean menores a 25 y mayores a 0.
 3. Recorrer el arreglo desde su segundo elemento hasta el ultimo, y para cada elemento imprimir lo siguiente:
    * Si el elemento es estrictamente mayor al elemento en posicion anterior, imprimir el caracter `+`
    * Si es estrictamente menor, imprimir el caracter `-`
    * Si es igual, imprimir `=`

Por ejemplo si el arreglo ingresado es `{3,3,4,5,1}`, se debe imprimir `=++-`.

El programa debe seguir andando si cambiamos `T` a valores mayores, por ejemplo `20`, `50`, etc.

# 3. Secuencia de Collatz (5 puntos)

Este ejercicio se hace en un archivo **collatz.c**.

 1. Definir una funcion `siguiente` que toma un entero que llamamos `n`.
    Si `n` es par, devuelve el valor `n/2`, sino devuelve `3*n + 1`.

 2. En la funcion `main`, hacer lo siguiente:
    1. Pedir al usuario un entero positivo `n` (en particular, seguir pidiendo hasta que
       el usuario ingrese un valor positivo).
    2. Luego, hasta que `n` valga `1`, reemplazar `n` por el resultado de la función `siguiente`
       aplicada a `n`, e imprimir este valor nuevo.

Por ejemplo, si el usuario ingresa `15`, deberiamos ver la salida siguiente:

~~~
15
46
23
70
35
106
53
160
80
40
20
10
5
16
8
4
2
1
~~~

 3. Modificar el programa para que, al final, indique la cantidad de veces
    que se modificó `n`. Por ejemplo con entrada `15`, deberiamos ver:


~~~
15
46
23
70
35
106
53
160
80
40
20
10
5
16
8
4
2
1

18 pasos
~~~

