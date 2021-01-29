% Recuperatorio 2: arreglos y funciones

Mandar los archivos por mail a <guillaumh@gmail.com>, como archivos adjuntos.

El parcial cuenta con 3 partes. Si se consigue la nota 0 en la parte 1 o la parte 2, no se regulariza el recuperatorio.

# 1. Arreglos (3 puntos)

Hacé este ejercicio en un archivo **arreglos.c**.

 1. Definir una constante `T` valiendo 5.
 2. Declarar un arreglo de `T` enteros.
 3. Pedir el ingreso de los `T` valores del arreglo al usuario, usando un bucle.
 4. Calcular e imprimir la suma de los elementos del arreglo que sean menores a 25 y mayores a 0.
 5. Recorrer el arreglo desde su segundo elemento hasta el ultimo, y para cada elemento imprimir lo siguiente:
    * Si el elemento es estrictamente mayor al elemento en posicion anterior, imprimir el caracter `+`
    * Si es estrictamente menor, imprimir el caracter `-`
    * Si es igual, imprimir `=`

Por ejemplo si el arreglo ingresado es `{3,3,4,5,1}`, se debe imprimir `=++-`.

El programa debe seguir andando si cambiamos la constante `T` a valores mayores, por ejemplo `20`, `50`, etc.

# 2. Funciones (4 puntos)

Hacé este ejercicio en un archivo **funciones.c** que tenga la función `main` siguiente:

~~~C
#define T 50

main(){
}
~~~

Es **obligatorio** que el archivo compile con `tcc` (por más que no haga nada cuando se ejecute).

Se trata de definir funciones sin llamarlas.

  * El archivo tiene que ejecutarse sin error con `tcc -run funciones.c`.
  * La constante `T` sirve para especificar el tamaño de todos los arreglos.
    Usarla cuando es necesario.
  * Si no está indicado que una función devuelve algun valor,
    es que no devuelve nada.
  * "Devolver" e "Imprimir" son dos cosas distintas.

 1. Definí una función `f1`, la cual recibe como parámetro un entero
    y devuelva el doble de su valor.
 2. Definí una función `f2`, la cual recibe como parámetros tres enteros
    y devuelva el promedio de ellos.
 3. Definí una función `f3` imprima el mensaje "Hola Mundo".
 4. Implementá una función `int buscar(const int arr[], const int elem)`, que
    realiza la búsqueda de un elemento en un arreglo.
    Esta función devuelve:
    * el valor `-1` cuando el valor buscado no
      está en el arreglo.
    * sino, el primer valor `i` tal que `elem == arr[i]`

# 3. Secuencia de Collatz (3 puntos)

En este ejercicio vas a implementar la famosa Secuencia de Collatz, una secuencia
que, dada cualquier número positivo mayor a 1, siempre vuelve a 1 (aunque todavía
no se sabe por qué).

Hacé este ejercicio en un archivo **collatz.c**.

 1. Definí una funcion `siguiente` que toma como parámetro un entero llamado `n`.
    Si `n` es par, devuelve el valor `n/2`, sino devuelve `3*n + 1`.

 2. En la funcion `main`, hacé lo siguiente:
    1. Pedí al usuario un entero positivo `n` (mejor si seguís pidiendo hasta que
       el usuario ingrese un valor positivo).
    2. Luego, hasta que `n` valga `1`, reemplazá `n` por el resultado de la función `siguiente`
       aplicada a `n`, e imprimí este valor nuevo.

Por ejemplo, si el usuario ingresa `15`, deberias ver la salida siguiente:

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
