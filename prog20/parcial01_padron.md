% Programación 1: parcial 1
% 2020-05-11

La nota total de este parcial - sobre 10 puntos - ya contiene
3 puntos de participación y entrega de las tareas semanales.

# Pruebas de escritorio (3 puntos)

Hacé las pruebas de escritorio siguientes por escrito, luego sacales fotos con tu celular y
mandales a <guillaumh@gmail.com> ni bien terminás.

## 1.1

~~~C
int a = 1;
if (a > 10){
    a = a - 15;
} else {
    a = a + 10;
} 
~~~

## 1.2

~~~C
int a = 10;
int b = 1;
int c = 0;
if (a <= 10 && b >= 1){
    c = c + 1;
}
if (a < 10){
    c = c + 1;
}
if (b > 1){
    c = c + 1;
} 
~~~


## 1.3

~~~C
int a = 99;
int c = 0;
if (a >= 80){
    c = c + 1;
} else if (a >= 90) {
    c = c + 1;
} else if (a >= 95) {
    c = c + 1;
}
~~~

## 1.4

~~~C
int b = 9;
int c = 0;
if (b > 10){
    c = 1;
} else if (b > 5){
    c = 2;
} else {
    c = 3;
}
~~~

## 1.5

~~~C
int i = 0;
int a = 0;
while (i < 5){
    i = i + 2;
}
~~~

## 1.6

~~~C
int i = 0;
int a = 0;
while (i < 4){
    if (i % 2 == 0)
        a = a + 1;
    else
        a = a - 1;
    i = i + 1;
}
~~~

## 1.8

~~~C
int i = 5;
int a = 0;
while (i < 4){
    if (i % 2 == 0)
        a = a + 1;
    else
        a = a - 1;
}
~~~


## 1.9

~~~C
int i = 0;
int a = 0;
while (i < 99){
    a = a + i;
    i = 100;
}
~~~

## 1.10

~~~C
int i = 1;
int a = 0;
while (i < 5){
    a = a + i;
    i = a;
}
~~~

## 1.11 

~~~C
int i = 0;
do
  i = i + 1;
while (i < 4);
~~~

## 1.12

~~~C
int i = 10;
do
  i = i + 1;
while (i < 4);
~~~

## 1.13

~~~C
int i = 3;
int a = 0;
do {
  i = i + 1;
  a = a + i;
}
while (i < 3);
while (i < 5){
 i = i + 2;
 a = a + i;
}
~~~


## 1.14

~~~C
int i=0;
int j=0;
while (i < 3){
  while (j < 3)
    j = j + 1;
  i = i + 1;
}
~~~

# Programas (4 puntos)

Los ejercicios se deben hacer dentro de [JSLinux](http://tinyurl.com/prog1linux),
y con el editor de texto `vi`.

Cada programa deberá llamarse como indicado en el enunciado.

Para descargarlos, usar dentro de jslinux el comando `export_file`.

## 2.1

Contestar cada problema en un programa distinto.

1. Realizá un programa que pida cargar tres números distintos, luego imprime el mayor de ellos.
2. Realizá un programa que pida cargar una fecha (primero el día, luego el mes), y luego verifique si dicha fecha corresponde a Navidad (imprime un mensaje según sea Navidad o no).

## 2.2 

Hacer un programa selectivo.c que imprima las secuencias siguientes:

1.    los enteros de 0 a 100 salvo los que son múltiples de 7.
2.    los enteros de 0 a 30 salvo los que son múltiples de 3.
3.    los enteros de 50 a 500 salvo los que son múltiples de 11

## 2.3

Hacer los ejercicios siguientes en jslinux:

1.    escribir un programa suma1.c que pida el valor entero m al usuario, y luego calcule e imprime la suma 1 + 2 + ... + m.
2.    escribir un programa suma2.c que pida los valores enteros n y m al usuario, y luego calcule e imprime la suma de todos los enteros entre 1 y n que no son divisibles por m. (Comprobar: si n=100 y m=2, la suma vale 2500).
3.    escribir un programa prod1.c que pida el valor entero n al usuario, y luego calcule e imprime el producto 1 * 2 * ... * n. (Comprobar: si n=5, el resultado es 120).
4.    escribir un programa prod2.c que pida el valor entero n al usuario, y luego calcule e imprime el producto n * (n + 1) * ... * (n + 5). (Comprobar: si n=5, el resultado vale 5 * 6 * 7 * 8 * 9 * 10 = 151200).


## 2.4

Escribir un programa que pide un entero n al usuario e imprima la secuencia de Collatz desde ese entero n hasta 1, y luego imprima cuántos pasos tiene la sucesión.

Consultar https://es.wikipedia.org/wiki/Conjetura_de_Collatz y usar los ejemplos 6, 11 y 27 para comprobar su programa.

## 2.5

En un programa promedio.c declarar las variables siguientes:

```
int i;
int veces;
int x;
float suma;
float promedio;
```

Hacer que este programa solicite un valor entero al usuario, y lo guarde en la variable veces. Luego pide al usuario veces veces un entero, suma estos enteros en la variable suma. Luego, calcula el promedio de esos enteros y la guarda en la variable promedio. Finalmente, imprime ese promedio, con 2 decimales de precisión (en printf, usar el código %.2f en lugar de %f).

Comprobar con los valores siguientes: Ingresar 3 (veces), luego los enteros 1, 3 y 10; el programa debería imprimir 4.67.


## 2.6

Escribí programas que permitan resolver los siguientes problemas. Podés agregar los mensajes apropiados para que el usuario se oriente:

1.    Ingresar dos valores numéricos, que representan la base y la altura de un rectángulo. Calcular y publicar su perímetro y superficie.
2.    Ingresar 4 datos enteros denominados I, J, K y L. Si I/J es igual a K/L, publicar un mensaje indicando que los cocientes son iguales. Verificar que ambos divisores sean distintos de cero.

En cada caso codificá la solución en lenguaje C.



## 2.7

Escribí programas que permitan resolver los siguientes problemas. Podés agregar los mensajes apropiados para que el usuario se oriente:

1.    Ingresar un valor n, luego n enteros, y determinar el mayor de todos.
2.    Ingresar n datos numéricos. Entre todos los datos mayores que 100, identificar y publicar el menor.

Codificá la solución para cada problema en lenguaje C.

## 2.8

Escribí programas que permitan resolver los siguientes problemas. Podés agregar los mensajes apropiados para que el usuario se oriente:

1.    Ingresar n datos numéricos. Identificar y publicar el mayor y el menor de todos, y en qué posición se encuentra cada uno
2.    Ingresar y sumar una serie de números positivos. La carga finaliza cuando se ingresa un valor negativo. Imprimir la suma resultante

Codificá la solución para cada problema en lenguaje C.

## 2.9


Juego “adivinar el número”

En este ejercicio vamos a programar un juego donde la computadora “elige” un número entre 1 y 10, y el jugador tiene que adivinarlo.

La estructura del programa es la siguiente:

    el programa “elige” un número n entre 1 y 10. Para lograr esto, usar la función time que devuelve un valor entero dependiendo de la hora actual en segundos:

    int n = (time(0) % 10) + 1;

    Para no tener warning, agregar al principio del programa #include <time.h>.
    el usuario ingresa un número u
    si u no es el número exacto, el programa dice si n es más grande o más chiquito que el número ingresado

    repetimos desde 2. hasta que u sea igual a n

Usemos un bucle para que el programa le pida un entero al usuario por lo menos una vez y se repita hasta que sea encontrada el entero n. Es más elegante usar un bucle do-while que while en este programa.

El programa tiene que imprimir los mensajes adecuados para informarle al usuario qué hacer y qué pasó.

Programar el juego en un archivo adivina.c.
