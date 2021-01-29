% Final Programación 1 - Octubre 2020 - UBP

Checklist:

* manden los ejercicios por mail a <guillaumh@gmail.com> **y** también súbanlos a MiUBP
* los programas entregados deben compilar con `tcc -run`
* el examen se evalua sobre 100 puntos, para aprobar se necesitan 50 puntos.
* la duración del examen es de 75 minutos (1 hora y 15 minutos)

# 1. Pruebas de escritorio (10 puntos)

Entregá este ejercicio hecho a mano, fotografiado.

~~~C
/* 1.1 */
int j = 2;
int a = 0;
if (j > a){
  a = a + 2;
}
if (j > a){
  j = j - 3;
} else {
  j = j + a + 1;
  a = j;
}
~~~

~~~C
/* 1.2 */
int i = 0;
do {
    if (i % 2 == 0)
      i = i + 1;
    else
      i = i + 2;
} while (i < 4);
~~~


# 2. Bucles (20 puntos)

Escribí un programa **intervalo.c** que pida al usuario dos números enteros,
y luego entregue la suma de todos los números que están **entre** ellos. Por ejemplo,
si los números son 1 y 7, debe entregar como resultado 2 + 3 + 4 + 5 + 6 = 20.

~~~
Ingrese num: 1
Ingrese num: 7
La suma es 20
~~~

Además, el programa deberá tener en cuenta lo siguiente: si el primer número ingresado
mayor al segundo, se debe dar el mismo resultado (intervertir los valores en ese caso):

~~~
Ingrese num: 7
Ingrese num: 1
La suma es 20
~~~

# 3. Tiempo de viaje (20 puntos)

Realizá esta parte en un programa llamado **tiempo.c**.

Un viajero desea saber cuánto tiempo tomó un viaje que realizó. Tiene la duración en minutos de cada uno de los tramos del viaje.

La idea es desarrolar un programa que permita ingresar los tiempos de viaje
de los tramos en minutos, y entregue como resultado el tiempo total
de viaje en formato horas:minutos.

Si se ingresa un 0, el programa deja de pedir tiempos de viaje.

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

3. Implementá la modificación siguiente: además de todo lo anterior, si se ingresa -1, se reinicializa la suma a cero y el programa vuelve a pedir
   duraciones de tramo al usuario. 

# 4. Arreglos (30 puntos)

Hacer este ejercicio en un archivo **arreglo.c**.

Hacer un programa defina una constante `T` valiendo 5, que permita ingresar un arreglo de `T` elementos, e informe:

1. La suma de los elementos del arreglo que sean mayores a 36.
2. La cantidad de valores mayores a 20.
3. Finalmente, imprime el contenido del arreglo desde su último elemento hasta su primero.

El programa debe seguir andando si cambiamos `T` a valores mayores, por ejemplo `20`, `50`, etc.

# 5. Funciones (20 puntos)

Hacé este ejercicio en un archivo **funciones.c** que tenga la función `main` siguiente:

~~~C
#include <stdio.h>
#define T 50

int main(){
  return 0;
}
~~~

Se trata de solo definir las funciones, no llamarlas.

  * El archivo tiene que ejecutarse sin error con `tcc -run funciones.c` (aunque no muestre nada, dado
    que el main está vacío).
  * La constante `T` sirve para especificar el tamaño de todos los arreglos. Usala cuando es necesario.
  * Si no está indicado que una función **devuelve** algun valor, es que no devuelve nada.


 1. Definir una función `f1`, la cual recibe como parámetros dos enteros
    y devuelva el mayor de ellos.
 2. Definir una función `f2` que toma un arreglo de enteros y
    que imprime todos sus elementos, separados por una coma, y al final terminado por un punto.
 3. Definir una función `f3` que toma como parametro un
    arreglo de enteros y un entero `e`. La función suma
    el entero `e` a cada uno de los elementos del arreglo,
    luego devuelve ese mismo entero `e` sin modificarlo.
 4. Definir una función `f4` que toma dos arreglos de enteros como
    parametros, calcula la suma de los elementos de cada uno, y devuelve la mayor
    suma de las dos.


