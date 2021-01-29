% Recuperatorio Programación 1: algoritmos

Los ejercicios se deben hacer dentro de [JSLinux](http://tinyurl.com/prog1linux),
y con el editor de texto `vi`.

Los programas entregados deben ejecutarse sin error con `tcc -run`.

Para regularizar el recuperatorio, es necesario tener por lo menos 1 punto
como mínimo en las partes 1 y 2.

# Parte 1: secuencias (4 puntos)

Realizá los cálculos siguientes en un solo archivo **secuencias.c**.

 1. Consideremos la serie que empieza con 11, 22, 33, 44, etc.
    Realizar un programa que imprima 25 términos de esa serie.
    (No se piden valores al usuario).

 2. Se ingresan 5 alturas de personas por teclado en cm.
    Mostrar la altura promedio de las personas, y también informar
    cuántas personas miden más de 150cm.

 3. Ingresar y sumar una serie de enteros entre 0 y 100. La carga finaliza cuando se
    ingresa un valor fuera de ese rango. Imprimir el promedio de los números ingresados,
    salvo si no se ingresó ningun número de ese rango.

# Parte 2: Tiempo de viaje (2 puntos)

Realizá esta parte en un programa llamado **tiempo.c**.

Un viajero desea saber cuánto tiempo tomó un viaje que realizó. Tiene la duración en minutos de cada uno de los tramos del viaje.

La idea es desarrolar un programa que permita ingresar los tiempos de viaje
de los tramos en minutos, y entregue como resultado el tiempo total
de viaje en formato horas:minutos.

El programa deja de pedir tiempos de viaje cuando se ingresa un 0.

Ejemplos:

~~~
Duracion tramo:
15
Duracion tramo:
30
Duracion tramo:
87
Duracion tramo:
0
Tiempo total de viaje: 2:12 horas
~~~

~~~
Duracion tramo:
51
Duracion tramo:
17
Duracion tramo:
0
Tiempo total de viaje: 1:08 horas
~~~

1. Implementá la parte del programa que pide los tiempos de viajes en minutos
   y los suma en una variable.

2. Implementá la parte del programa que imprime esta suma en el formato horas:minutos.

# Parte 3: Fibonacci (4 puntos)

La serie de Fibonacci es la siguiente sucesión infinita de números naturales.
La sucesión comienza con los números 0 y 1, partir de estos, cada término es
la suma de los dos anteriores:

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597 ...

Si tenemos dos elementos consecutivos `x` e `y`, conseguimos el valor del
elemento siguiente calculando `x+y`. Luego, el valor `y` pasa a ser el nuevo valor
de la variable `x`, y el valor `x+y` pasa a ser el valor de la variable `y`.

Escribí un programa **fibo.c** que consiste en lo siguiente:

1. Declará las siguientes variables:

~~~C
int i;       // variable de conteo
int x=0,y=1; // los primeros elementos de la lista
int tmp;     // variable temporaria para actualizar x e y
int n;       // entero ingresado por el usuario
~~~

2. Con un bucle, actualizá las variables `x` e `y` para avanzar en la secuencia,
   hasta que `x`. El bucle se ejecuta 30 veces y luego, se imprime el valor de `x`.
   Debe dar `832040`.
3. Modificá este programa para que antes del bucle, pida al usuario un entero `n`.
   Si ese entero es negativo, se ejecuta el cálculo anterior sin cambios.
   Si ese entero es positivo, se busca si `n` es un número de fibonacci, es decir,
   se chequea si `n` pertenece a dicha secuencia. El bucle de la secuencia termina
   si el valor de `x` supera el de `n`. Se debe anunciar si sí o no, el valor
   ingresado es número de fibonacci.
