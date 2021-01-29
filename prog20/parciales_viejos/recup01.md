% Recuperatorio 2: pruebas de escritorio y algoritmos

Entregar las respuestas sobre papel.
No se puede consultar ningun apunte.

# 1. Pruebas de escritorio (3 puntos, 1 punto mínimo obligatorio)

Para cada uno de los programas siguientes, escribí la prueba de
escritorio. Ojo que en la columna de la izquierda solo aparecen
sentencias (incluso las condiciones), pero no aparecen las palabras
`if`, `else`, `while`, `for`, etc. 

~~~C
/* 1.1 */
int a = 1;
if (a > 10){
    a = a - 15;
} else {
    a = a + 10;
}
if (a != 10)
    a = a + 4;
~~~

~~~C
/* 1.2 */
int a = 10;
int c = 0;
if (a <= 10){
  while(a > 0)
    a = a - 5;
  c = c + 1;
}
~~~

~~~C
/* 1.3 */
int b = 7;
int c = 0;
if (b > 9){
    c = 1;
} else if (b > 5){
    c = 2;
} else {
    c = 3;
}
~~~

# 2. Salida de programas (2 puntos)

En este ejercicio escribí solamente la salida de cada programa.

~~~C
/* 3.1 */
int i = 0;
while (i < 6){
    if ( i % 2 == 0)
      putchar('+');
    else
      putchar('-');
    i = i + 1;
}
~~~

~~~C
/* 3.2 */
int i;
int a=2;
for(i = 0; i <= 6; i = i + 1)
    a = a + 1;
printf("%d,%d", a, i, j);
~~~

# 3. Algoritmos (5 puntos, 1 punto mínimo obligatorio)

Para cada uno de los enunciados siguientes, escribí un algoritmo
que lo implemente.

 1. Consideremos la serie que empieza con 11, 22, 33, 44, etc.
    Realizar un programa que imprima 25 términos de esa serie.
    (No se piden valores al usuario).

 2. Se ingresan 20 alturas de personas por teclado.
    Mostrar la altura promedio de las personas. 

 3. Escribir un programa que lea 30 notas de alumnos y nos informe cuántos tienen notas
    mayores o iguales a 7 y cuántos menores.

 4. Ingresar $n$ y $m$. Calcular e imprimir $\sum_{i=1}^n (i+m)$.

 5. Ingresar y sumar una serie de enteros entre 0 y 100. La carga finaliza cuando se
    ingresa un valor fuera de ese rango. Imprimir el promedio de los números ingresados,
    salvo si no se ingresó ningun número de ese rango.


## 2.4 Fibonacci

En matemáticas, la serie de Fibonacci es la siguiente sucesión infinita de números naturales:

0 , 1 , 1 , 2 , 3 , 5 , 8 , 13 , 21 , 34 , 55 , 89 , 144 , 233 , 377 , 610 , 987 , 1597 ...

La sucesión comienza con los números 0 y 1, partir de estos, cada término es la suma de los dos anteriores.

Si tengo dos elementos consecutivos `x` e `y`, consigo el valor del elemento siguiente calculando `x+y`.

Escribir un programa **fibo.c** que consiste en lo siguiente:

* declara las siguientes variables:

~~~C
int i=0;     // variable de conteo
int x=0,y=1; // dos elementos consecutivos de la lista
int z;       // variable temporaria para calcular el siguiente elemento
~~~


* con un bucle, actualizar las variables `x` e `y` para avanzar en la secuencia, hasta que x sea el elemento 50
  de la serie, e y el elemento 51
* imprimir estos dos valores

## 2.1 Programa simple (0.5 punto)

Escriba un programa **hora.c** que pregunte al usuario la hora actual `t` del reloj
y un número entero de horas `h`, que indique qué hora marcará el reloj dentro de `h` horas:

~~~
Hora actual: 3
Cantidad de horas: 5
En 5 horas, el reloj marcara las 8
~~~

~~~
Hora actual: 11
Cantidad de horas: 43
En 43 horas, el reloj marcara las 6
~~~


