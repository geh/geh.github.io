% Programación 1: Parcial 2
% Lunes 22/05/2017

Entregar las respuestas sobre papel. No se puede consultar
ningun apunte.

# 1. Pruebas de escritorio (2 puntos)

Para cada uno de los programas siguientes, escribí la prueba de
escritorio. Ojo que en la columna de la izquierda solo aparecen
sentencias (incluso las condiciones), pero no aparecen las palabras
`if`, `else`, `while`, `for`, etc. 

~~~C
/* 1.1 */
int i = 2;
if (i > 0){
  i = i + 2;
}
if (i > 3){
  i = i - 3;
}
~~~

~~~C
/* 1.2 */
int i = 2;
while (i < 4)
    i = i + 1;
~~~

~~~C
/* 1.3 */
int i = 1;
int a = 2;
for(i=3; i <= 5; i = i + 2)
    a = a * a;
~~~

~~~C
/* 1.4 */
int i = 0;
int a = 8;
do {
 i = i + 5;
 a = a - 3;
} while (i < a)
~~~

# 2. Más pruebas de escritorio (2 puntos)

Misma consigna que el ejercicio anterior.

~~~C
/* 2.1 */
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
/* 2.2 */
int a = 10;
int c = 0;
if (a <= 10){
  while(a > 0)
    a = a - 5;
  c = c + 1;
}
~~~

~~~C
/* 2.3 */
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

~~~C
/* 2.4 */
int i = 0;
int a = 0;
int b = 0;
while (i < 4){
    if (i%2==0)
        a = a + 1;
    else
        b = b + 1;
    i = i + 1;
}
~~~

# 3. Salida de programas (2 puntos)

En este ejercicio escribí solamente la salida de cada programa.

~~~C
/* 3.1 */
int i = 0;
while (i < 4){
    putchar('x');
    i = i + 1;}
printf("\no\n");
~~~

~~~C
/* 3.2 */
int i, j;
int a=0;
for(i = 0; i <= 6; i = i + 1)
  for(j = 0; j <= 6; j = j + 1)
    a = a + 1;
printf("%d,%d,%d", a, i, j);
~~~

~~~C
/* 3.3 */
int i=0;
int a=0;
do{ if (i % 2 == 0)
        a = a + 1;
    i = i + 2;
} while(i < 10);
printf("%d,%d",i,a);
~~~

# 3. Algoritmos (5 puntos)

Para cada uno de los enunciados siguientes, escribí un algoritmo
que lo implemente. Tiene que reflejar la *estructura*
del programa C correspondiente.

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



