% Funciones

C tiene como bloque básico la función `main()`, también hemos visto la sentencia
`printf()` que es otra función, y de igual forma hay muchas más funciones
predefinidas, pero nosotros mismos también podemos definir nuestras
propias funciones. De hecho, es fundamental hacerlo.

Podemos definir una función cualquiera de la misma manera en que
definimos la función `main()`.

Basta con poner su tipo, su nombre, sus argumentos entre paréntesis y
luego, entre llaves, su código:

``` {.c}
#include <stdio.h>

void holamundo(void) /* Función donde se ejecuta la lógica del programa */
{
    printf("Hola Mundo\n"); /* imprime la cadena */
    return; /* sale de la función */
}
 
int main(void) /* Función principal del programa */
{
    holamundo(); /* llamada a la función holamundo */
    return 0; /* sale del programa con código 0 (correcto) */
}
```

Este código es en todo equivalente al \"Hola Mundo\" original, sólo que
nos muestra cómo escribir y cómo utilizar una función. Y además nos
muestra un principio de buena programación: meter las sentencias que
"hacen el trabajo" en otras funciones específicas para sacarlas de
`main()`, dejando en ésta tan sólo un guión general de lo que hace el programa,
no las órdenes específicas. De esta manera se facilita la comprensión
del programa, y por tanto el futuro trabajo de modificarlo.

# La sentencia `return` #

La sentencia `return` puede utilizarse dentro de una función para terminar su ejecución.

En el ejemplo anterior, la función `holamundo` fue declarada con valor de retorno de tipo `void`.
(es decir, valor de retorno nulo). En ese caso, la sentencia `return` no lleva ningún parámetro adicional, ya que la función no debe devolver
ningún valor a la función que la llama.

En cambio, la función `main` tiene un valor de retorno de tipo `int`,
por lo que `return`
debe ir seguido de un valor entero (0 en el ejemplo). El valor 0 se
utiliza para indicar que el programa ha llegado a un punto en el que
todo se ha desarrollado correctamente y se utiliza cualquier otro valor
para indicar que ha habido algún tipo de error.

La instrucción `return`
no es una función, se trata de una sentencia que lo que hace es retornar
como valor de la función el valor que se le proporciona como argumento.

# Parámetros

Las funciones también pueden recibir argumentos o parámetros, para
modificar su comportamiento. Por ejemplo, la definición de una función
para sumar dos números sería de la siguiente manera:

``` {.c}
#include <stdio.h>

int sumar(int numero1, int numero2)
{
    return numero1 + numero2;
}

int main(void)
{
    int suma = sumar(5, 3); /* Se ejecuta correctamente*/
    printf("La suma es: %d ", suma);
    return 0;
}
```

# Declaración y definición

En el ejemplo anterior podemos notar que la función `sumar`
figura en el código antes que `main`.

¿Qué pasaría si las escribiéramos en distinto orden?

``` {.c}
#include <stdio.h>

int main(void)
{
    int suma = sumar(5, 3); /* ERROR, sumar no ha sido declarada aún */
    printf("La suma es: %d ", suma);
    return 0;
}

int sumar(int numero1, int numero2)
{
    return numero1 + numero2;
}
```

En este caso el programa es erróneo y no compila, ya que en la línea
donde se llama a la función `sumar`,
el compilador aún no conoce ninguna función con ese nombre, y cuáles
son sus argumentos y valor de retorno.

Una posible solución es **declarar** el **prototipo** de la función al
principio, para informar al compilador que existe, y luego **definir**
el **cuerpo** de la misma en cualquier lugar del programa:

``` {.c}
#include <stdio.h>

/* Declaración */
int sumar(int numero1, int numero2);

int main(void)
{
    int suma = sumar(5, 3);
    printf("La suma es: %d ", suma);
    return 0;
}

/* Definición */
int sumar(int numero1, int numero2)
{
    return numero1 + numero2;
}
```

# Paso de Parámetros

Las funciones pueden recibir datos como lo hemos observado, pero existen
dos formas de enviar los datos hacia una función **por valor** y **por
referencia**, las cuales modifican en diferente forma el comportamiento
del programa.

## Por Valor

El paso por valor envía una copia de los parámetros a la función por lo
tanto los cambios que se hagan en ella no son tomados en cuenta dentro
de la función `main()`. Ejemplo:

``` {.c}
#include <stdio.h>

void sumar_valor(int numero); /* prototipo de la función */

int main(void)
{
    int numero = 57; /* definimos numero con valor de 57*/

    sumar_valor(numero); /* enviamos numero a la función */

    printf("Valor de numero dentro de main() es: %d\n", numero);
    /* podemos notar que el valor de numero se modifica
     * sólo dentro de la función sumar_valor pero en la principal
     * número sigue valiendo 57 
     */

    return 0;
}

void sumar_valor(int numero)
{
    numero++; /* le sumamos 1 al numero */

    /* el valor de número recibido se aumenta en 1
     * y se modifica dentro de la función sumar_valor()
     */
    printf("Valor de numero dentro sumar_valor() es: %d\n", numero);

    return;
}
```

## Por Referencia

El paso por referencia se hace utilizando apuntadores. Se envía la
dirección de memoria de la variable, por lo tanto los cambios que haga
la función si afectan el valor de la variable. Ejemplo:

``` {.c}
#include <stdio.h>

void sumar_referencia(int *numero); /* prototipo de la función */


int main(void)
{
    int numero = 57; /* definimos numero con valor de 57*/

    sumar_referencia(&numero); /* enviamos numero a la función */

    printf("\nValor de numero dentro de main() es: %d ", numero);
    /* podemos notar que el valor de numero se modifica
     * y que ahora dentro de main() también se ha modificado
     * aunque la función no haya retornado ningún valor.
     */

    return 0;
}

void sumar_referencia(int *numero)
{
    *numero += 1; /* le sumamos 1 al numero */

    /* el valor de numero recibido se aumenta en 1
     * y se modifica dentro de la función
     */
    printf("\nValor de numero dentro sumar_referencia() es: %d", *numero);

    return;
}
```

# Variables Locales y Globales

Además de pasar valores a una función, también se pueden declarar tipos
de datos dentro de las funciones, estos tipos de datos declarados dentro
de una función solo son accesibles dentro de esta misma función y se les
conocen como variables locales, así pues podemos definir los mismos
nombres de variables en diferentes funciones, ya que estas variables
solo son accesibles dentro de esas funciones. Ejemplo:

``` {.c}
#include <stdio.h>

void funcion1()
{
    int dato = 53; /* definimos dato en 53*/
    char num1 = 'a'; /* num1 vale a */

    /* imprimimos */
    printf("Funcion1, dato=%d, num1=%c\n", dato, num1);

    return;
}

void funcion2()
{
    int dato = 25; /* definimos dato en 25*/
    char num2 = 'z'; /* num2 vale z*/

    /* imprimimos */
    printf("Funcion2, dato=%d, num2=%c\n", dato, num2);

    return;
}

int main(void)
{
    funcion1(); /* llamamos a funcion1() */

    funcion2(); /* llamamos a funcion2() */

    return 0;
}
```

En este caso la variable dato, esta definida dentro de cada una de las
funciones y son totalmente distinta una de otra y no se puede utilizar
fuera de esta, así pues num2 no puede ser utilizada por la funcion1() y
num1 tampoco puede ser utilizada por funcion2().

Existen pues variables que se definen fuera de la función principal
main() y fuera de cualquier otra función creada por nosotros, estas
variables se les conoce con el nombre de Variables Globales ya que se
pueden utilizar dentro de main() y dentro de cualquier función creada
por nosotros. Ejemplo:

``` {.c}
#include <stdio.h>

int variable_global = 99; /* inicializamos la variable global */

void funcion();

int main(void)
{
    /* imprimimos el valor*/
    printf("main(), acceso a variable_global %d\n", variable_global);

    /* llamamos a la función */
    funcion();

    return 0;
}

void funcion()
{
    /* imprimimos el valor*/
    printf("funcion(), acceso a variable_global %d\n", variable_global);

    return;
}
```

# Noción de *parametro formal* y *parametro real*.

Ejemplo:

~~~C
#include <stdio.h>

void hola(void){
  printf("Hola mundo!\n");
}

void mostrar1(int a){
  printf("Tengo %d manzanas.\n", a);
}

void mostrar2(int a, int b){
  printf("Tengo %d manzanas y vos %d.\n", a, b);
}

int main(void) {
  int x = 60;

  hola();

  mostrar1(18);
  mostrar1(20);

  mostrar1(x);

  mostrar2(20,25);
  mostrar2(25,20);

  mostrar2(x,x);
  mostrar2(x,100);

  return 0;
}
~~~

# Funciones que devuelven valores

Ver en el apunte "Aprenda C", el uso de la palabra-clave `return`.

~~~C
#include <stdio.h>

int cuadrado(int a){
  return a*a;
}

int pi(void){
  return 3;
}

int main(void){
  int x = 0;
  x = pi();
  x = cuadrado(5);
  printf("x vale %d\n", x);
  return 0;
}
~~~

# Definir funciones Booleanas

En el lenguaje C se interpetan los valores enteros no nulos como "verdaderos"
y nulos como "falsos".

Es decir, la condición siguente siempre es **falsa**:

~~~C
if ( 0 ) {
  ...
}
~~~

Las condiciones siguientes siempre son **verdaderas**:

~~~C
if ( 1 ) {
  ...
}
while ( 2 ) {
  ...
}
for ( ... ; 3 ; ... ){
  ...
}
~~~

Pero se suele usar el valor "1" para indicar "verdadero".

# Funciones que toman arreglos como parametros #

Cuando indicamos que una función toma un arreglo como parametro,
no es necesario indicar el tamaño de ese último y se puede indicar
de la forma siguiente: `int a[]` (si queremos llamar `a` el arreglo).

Además si queremos decirle al compilador que la función no debe
modificar valores del arreglo, agregamos la palabra `const`.
Por ejemplo `void mostrar(const int a[])`.

O si consideramos la funcion que, dada una lista,
nos devuelve su máximo elemento:

~~~C
#include <stdio.h>
#define MAX 6

int max_lista(const int a[]){
    int i=0;
    int m= a[0];
    for(i=1;i<MAX;i++){
        if (a[i] > m){ m = a[i]; }
    }
    return m;
}

int main(void){
    int a[MAX]= {45,6,2,36,6236,3};
    int m;
    m = max_lista(a);
    printf("%d\n", m);
    return 0;
}
~~~

Si queremos escribir funciones que toman arreglos como
parámetros y los modifican, sacamos la palabra `const` para permitirlo.

<div class="info">
Este cuatrimestre no vamos a escribir funciones que *devuelven*
arreglos nuevos. Eso implicaría gestionar la memoria de manera
dinámica.

En lugar de devolver un arreglo nuevo, vamos a pasar como parametro
un arreglo vacío para que reciba el resultado de la función,
y ponemos que el tipo de devolución de la función es `void`.

Por ejemplo, en la función siguiente consideramos que el
arreglo `b` es el resultado del cálculo:

~~~C
void mas_uno(const int a[], int b[]){
 int i;
 for(i=0; i<MAX; i=i+1){
  b[i] = a[i] + 1;
 }
}

~~~

¿Qué hace esta función?
</div>

# Algoritmo de búsqueda, uso de la sentencia `return` #

Primero, leer los apuntes siguientes:

* En "Aprenda C", capítulo 7 "Funciones", leer las secciones 7.1, 7.2 y 7.3. 
* En el wikilibro "Programación en C", leer el capítulo
  ["Uso de funciones"](https://es.wikibooks.org/wiki/Programaci%C3%B3n_en_C/Uso_de_funciones)
  hasta el párrafo "la sentencia `return`".


