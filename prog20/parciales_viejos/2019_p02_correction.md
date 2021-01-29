% Programación 1: correccion del Parcial 4 sobre funciones

Pautas para todos los ejercicios:

  * Escribir las funciones sobre papel, usando la sintaxis del lenguaje C.
  * Está definida una constante `T` que indica el
    tamaño de todos los arreglos. Usarla cuando es necesario.
  * Si no está indicado que una función devuelve algun valor,
    es que no devuelve nada.
  * Acordarse que "Devolver" e "Imprimir" son dos cosas distintas.

# Acerca de las correcciones

Las respuestas dadas en este documento son correctas pero no son las únicas
respuestas correctas. Lo que puede variar son los nombres de los parámetros
(cuando el enunciado no los especifica) y las formas de realizar los cálculos
(ciertas condiciones o bucles podrían ser simplificados).

# Ejercicios corregidos

 1. Definir una función `f1` a la cual le envíe el valor del lado
    de un cuadrado y devuelva su perímetro.

~~~
int f1(int lado){
  return lado*4;
}
~~~

 2. Definir una función `f2` a la cual le envíe tres enteros
    y devuelva el mayor de ellos.

~~~
int f2(int x, int y, int z){
 if (x >= y && x >= z){
   return x;
 } else if (y >= x && y >= z){
   return y;
 } else
   return z;
}
~~~

 3. Definir una función `f3` que toma un caracter `d` y un entero `a`.
    Si `a` es mayor o igual a cero, la función devuelve `d`.
    Sino devuelve `d+1`.

~~~
int f3(char d, int a){
  if (a >= 0)
   return d;
  else
   return d+1;
}
~~~

 4. Definir una función `f4` que recibe dos parametros enteros
    `x` e `y` , imprime el valor de `x` multiplicado por `y`,
    luego devuelve `x`.

~~~
int f4(int x, int y){
 printf("%d", x*y);
 return x;
}
~~~

 5. Definir una función `f5` que recibe un parametro entero `n`
    e imprime todos los valores enteros desde `1` hasta `n` (inclusive).

~~~
void f5(int n){
   int i;
   for(i=0;i<=n;i=i+1)
     printf("%d", i);
}
~~~

 6. Definir una función `f6` que toma un arreglo de enteros y
    que imprime todos sus elementos.

~~~
void f6(int a[]){
  int i;
  for(i=0;i<=T;i=i+1)
     printf("%d", a[i]);
}
~~~

 7. Definir una función `f7` que toma como parametro un
    arreglo (de enteros) y un entero `e`. La función suma
    el entero `e` a cada uno de los elementos del arreglo,
    luego devuelve el mismo entero `e`.

~~~
int f7(int a[]){
  int i;
  for(i=0;i<=T;i=i+1)
    a[i] = a[i] + e;
  return e;
~~~

 8. Definir una función `f8` que toma dos arreglos de enteros como
    parametros, calcula la suma de los elementos de cada uno,
    y devuelve la mayor suma de las dos.

~~~
int f8(int a[], int b[]){
  int i, sa, sb;
  sa = 0;
  sb = 0;
  for(i=0;i<=T;i=i+1)
    sa = sa + a[i];
  for(i=0;i<=T;i=i+1)
    sb = sb + b[i];
  if (sa >= sb)
    return sa;
  else
    return sb;
~~~


