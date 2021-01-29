% Programación 1 - Parcial 2: arreglos y funciones
% 2020-06-22

Los ejercicios se deben hacer dentro de [JSLinux](http://tinyurl.com/prog1linux),
y con el editor de texto `vi`.

El código del ejercicio se deberá mandar al link que le pasará el profesor.

En cada ejercicio se debe tener por lo menos 1 punto para regularizar el parcial.

**No** es necesario que los programas compilen sin warnings con `gcc -Wall`, sin embargo
recomiento que prueben de vez en cuando para detectar posibles errores.

# 2. Funciones (4 puntos)

Hacé este ejercicio en un archivo `funciones.c` que tenga la función `main` siguiente:

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

 1. Definí una función `mayor`, la cual recibe como parámetros tres enteros
    y devuelva el mayor de ellos.
 2. Definí una función `f1` que toma un caracter `d` y un entero `a`.
    Si `a` es mayor o igual a cero, la función devuelve `d`.
    Sino devuelve `d+1`.
 3. Definí una función `f2` que recibe dos parametros enteros
    `x` e `y` , imprime el valor de `x` multiplicado por `y`,
    luego devuelve `x`.
 4. Definí una función `f3` que toma un arreglo de enteros y
    que imprime todos sus elementos, separados por una coma,
    y al final terminado por un punto.
 5. Definí una función `sumar1` que toma como parámetros un
    arreglo de enteros y un entero `b`. La función suma
    el entero `b` a cada uno de los elementos del arreglo,
    luego devuelve ese mismo entero `b` sin modificarlo.
 6. Implementá una función `sumar2`, que
    recibe un arreglo de enteros como parámetro, y realiza la suma de todos los elementos del arreglo dado como parámetro,
    y la devuelve.
