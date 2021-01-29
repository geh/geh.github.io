% Final Programación 1 - Julio 2020 - UBP

Checklist:

* manden los ejercicios por mail a <guillaumh@gmail.com> **y** también súbanlos a MiUBP
* los programas entregados deben compilar con `tcc -run`
* el examen se evalua sobre 100 puntos, para aprobar se necesitan 50 puntos.
* la duración del examen es de 90 minutos (1 hora y media)

# Parte 1: Bucles y condiciones (40 puntos)

## 1.1 Bucles (10 puntos)

Escribí un programa **intervalo.c** que pida al usuario dos números enteros,
y luego entregue la suma de todos los números que están entre ellos. Por ejemplo,
si los números son 1 y 7, debe entregar como resultado 2 + 3 + 4 + 5 + 6 = 20.

~~~
Ingrese num: 1
Ingrese num: 7
La suma es 20
~~~

Si el primer número es mayor al segundo, se debe dar el mismo resultado (intervertir los valores en ese caso):

~~~
Ingrese num: 7
Ingrese num: 1
La suma es 20
~~~

## 1.2 secuencias (10 puntos)

Realizá lo siguiente en un archivo **secuencias.c**:

Ingresar y sumar una serie de enteros entre 0 y 100. La carga finaliza cuando se
ingresa un valor fuera de ese rango. Imprimir el promedio de los números ingresados,
*salvo* si no se ingresó ningun número de ese rango.

## 1.3 Tiempo de viaje (20 puntos)

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

# Parte 2: Arreglos (35 puntos)

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

# Parte 3: Funciones (25 puntos)

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

 1. Definí una función `mayor`, la cual recibe como parámetros dos enteros
    y devuelva el mayor de ellos.
 2. Definí una función `f1` que recibe dos parametros enteros
    `x` e `y` , y solo si son distintos, imprime el valor de `x` multiplicado por `y`,
    luego devuelve `x`.
 3. Definí una función `f2` que toma un arreglo de enteros y
    que imprime todos sus elementos, separados por una espacio,
    y al final termina con un salto de línea.
 4. Definí una función `sumar1` que toma como parámetros un
    arreglo de enteros y un entero `b`. La función suma
    el entero `b` a cada uno de los elementos del arreglo,
    luego devuelve la suma de los elementos originales del arreglo.
 5. Implementá una función `sumar2`, que
    recibe un arreglo de enteros como parámetro, y realiza la suma de todos los elementos del arreglo dado como parámetro,
    y la devuelve.
