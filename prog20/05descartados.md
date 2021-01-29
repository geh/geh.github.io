
# Parte 2: Valores aleatorios

## 2.1 Inicializar y utilizar el generador de números aleatorios

Crear una copia del programa del ejercicio 2, llamada `alea.c`.

Modificar esta copia nueva de forma que no se use más scanf, sino que se consiguen
valores *aleatorios* para las dos variables.

El cuerpo del `main` deber conformarse de la forma siguiente:

* declaracion de las variables al principio
* luego inicializacion del generador aleatorio con la sentencia `srand(time(0))`
  (esto no es C muy estándar, `tcc` lo acepta tal cual, `gcc` emite un warning.
  Lo correcto es escribir `srand(time(NULL))` pero por ahora no nos cambia nada.)
* luego el resto del programa. Cada vez que hace falta conseguir un valor aleatorio, usar la expresión
  `rand()`. Por ejemplo para asignar un valor aleatorio a la variable `z`, usar la sentencia `z = rand()`.

~~~C
main(){
  int a;
  srand(time(0));
  a = rand();
  printf("este es un valor aleatorio:\n", a);
~~~

Este programa dejó de ser interactivo, ya no necesitamos darle valores manualmente, ¡los consigue solo!


## 2.2 Variables de acumulación y valores aleatorios

¿Qué proporción de todos los enteros naturales son divisibles por 7?

Ya lo sabemos, son 1 de cada 7. Pero podemos comprobarlo con un programa que genera
valores aleatorios y comprueba, para cada uno, si cumple con alguna condicion (acá, ser divisibles por 7).

Hacer un programa **buclealea.c** que hace esto.

Antes del bucle, inicializamos dos variables que nos van a servir de contadores: `si` y `no`.:

~~~C
int si = 0;
int no = 0;
~~~

En el bucle, que se deberá repetir 1000 veces, cada vez que el numero aleatorio cumple con la condicion, incrementamos `si`, sino, incrementamos `no`.

Despues del bucle, usar la sentencia siguiente para mostrar el porcentaje de números divisibles por 7:

~~~C
printf("proporcion: %d%%\n", (100*si) / (si+no));
~~~

## 2.3. Juego "adivinar el número"

En este ejercicio vamos a programar un juego donde la computadora
elige un número al azar entre 1 y 10, y el jugador tiene que adivinarlo.

La estructura del programa es la siguiente:

1. el programa elige al azar un número **n** entre 1 y 10
2. el usuario ingresa un número **u**
3. si **u** no es el número exacto, el programa dice si **n**
   es más grande o más chiquito que el número ingresado
4. repetimos desde 2. hasta que **u** sea igual a **n**

Usemos un bucle para que el programa le pida un entero al usuario por lo menos una vez
y se repita hasta que sea encontrada el entero **n**.

El programa tiene que imprimir los mensajes adecuados para
informarle al usuario qué hacer y qué pasó.

Programar el juego en un archivo **adivina.c**.

# Parte 3: Ejercicios no obligatorios


## 3.1: Volviendo a la línea de comando con `watch`

`watch` es un comando que sirve para repetir otros comandos. Es interesante para repetir comandos
cuya salida puede cambiar en el tiempo. Por ejemplo para ejecutar `ls -l` cada 2 seguntos (el intervalo
de repetición por defecto), se ejecuta el comando:

~~~bash
watch ls -l
~~~

Para salir de `watch`, apretar la combinación de teclas Control + c (en línea de comando, control+c no sirve
para copiar texto sino para matar el proceso corriente).

`watch` acepta el flag `-n` para especificar el intervalo de ejecución.
Para ejecutar el comando `date` cada 5 segundos:

~~~bash
watch -n 5 date
~~~

Ejecutar algun programa non-interactivo que genere y use números aleatorios cada 1 segundo, usando adecuadamente `watch` y `tcc`.

# Parte 4: El operador `&`, mostrar las direcciones en memoria de las variables

![cantidad de participantes](group64.png){width=1cm} 1 o 2 participantes.

 1. Crear un programa llamado `direccion.c`.
    Declarar una variable de tipo `char` con nombre `c`,
    y otra de tipo `int` con nombre `x`, sin asignarles valores iniciales.
    Mostrar su dirección en memoria, usando `printf`, el especificador de
    conversión `%p` y el operador `&`.

 2. Ejecutar el programa varias veces y observar la salida.

## 3.2. Variables de tipo `char *`

![cantidad de participantes](group64.png){width=1cm} 1 o 2 participantes.

 1. Consideramos el programa siguiente:

    ~~~
    main(){
      printf("Hey!\n");
    }
    ~~~

    Modificarlo (pero manteniendo su salida) para que la línea del `printf`
    sea como sigue:

    ~~~
      printf(mensaje);
    ~~~

    Para eso, declarar una variable nueva de tipo `char *` y de nombre
    `mensaje`.

 2. Consideramos el programa `queen.c` siguiente:

    ~~~
    main(){
      printf("%s all,%s all, %s all,and %s now\n",
             "I want it", "I want it", "I want it", "I want it");
    }
    ~~~

    a. Modificar este programa de forma que la constante cadena de caracteres
      `"I want it"` aparezca una sola vez en el código. Para eso definir
      una variable nueva.
    b. Definir una segunda variable de tipo cadena de caracteres para que
       la llamada a la función `printf` no tenga ninguna constante cadena
       de caracteres.


