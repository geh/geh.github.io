% Sorting




## Algoritmos de ordenamiento de listas ##

* ¿Qué es una lista ordenada?

Ordenada significa que cada dos elementos consecutivos,
están ordenados según la relación de orden $\leq$.

<div class="papel">
Definir un algoritmo `función es_ordenada(lista, tamaño)`
que devuelve "verdadero" si la lista dada es ordenada
y "falso" sino.
</div>

<div class="compu">
En [**listas.c**](http://hub.darcs.net/gh/prog1/raw-file/contenidos/src/listas.c)
definir la función `int es_ordenada(const int a[], const int size)` y usarla en el main (mostrar un mensaje
distinto si es ordenada o no).
</div>

## Algoritmos de ordenamiento de lista ##

Existen muchos algoritmos de ordenamiento de listas.
Se distinguen por varias características:

* ciertos operan sin usar memoria externa (son *in situ*),
  mientras otros sí requieren más memoria
* ciertos tienen un mejor rendimiento que otros en velocidad
* ciertos son más faciles de entender que otros
* hay unos que son recursivos y otros que son iterativos

Vamos a ver algoritmos *in situ* que son fáciles de entender.
Primero vemos el *selection sort* y luego el *gnome sort*.

## Selection sort ##

Tenemos una lista no necesariamente ordenada.
Una descripción de alto nivel del algoritmo sería:

* buscar el mínimo elemento de la lista
* intercambiarlo con el primero
* buscar el siguiente mínimo en el resto de la lista (del segundo elemento al último)
* intercambiarlo con el segundo
* buscar el siguiente mínimo en el resto de la lista (del tercero al último)
* intercambiarlo con el tercero
* Repetir hasta el final de la lista

A medida que progresa el proceso, el tramo inicial de
la lista se vuelve ordenado. Al final del proceso, toda la
lista se encuentra ordenada.

En este video se puede ver un ejemplo de selection sort con naipes (no hace falta sonido):

<iframe width="420" height="315" src="https://www.youtube-nocookie.com/embed/boOwArDShLU?rel=0&amp;showinfo=0" frameborder="0" allowfullscreen></iframe>

<div class="papel">
Escribir este algoritmo en pseudocódigo.

Necesitamos usar dos índices:

* $i$ que indica la próxima posición que tiene que recibir
  el mínimo del resto de la lista
* $j$ que usamos para recorrer el resto (desordenado) de la lista
  para buscar el mínimo.

El algoritmo tiene un primer bucle que hace variar el índice $i$
(¿de cuánto a cúanto?).

Adentro, tiene otro bucle que hace variar el índice $j$
(¿de cuánto a cuánto?).  Dentro de este segundo bucle, usamos una
variable $iMin$ para guardar el índice del elemento mínimo.
</div>

<div class="compu">
Implementar el algoritmo en una función `void selection_sort(int a[], const int size)` y
usarlo en el programa que tenemos.

Mostrar en la shell la lista después del sort para asegurarse que queda
ordenada.
</div>

<div class="compu">
Dentro de la función `selection_sort` imprimir la lista después de cada
intercambio de valores y observar la ejecución del programa.
</div>

## Observaciones ##

Sea $n$ el tamaño de la lista, el algoritmo selection sort siempre
va a hacer la misma cantidad de comparaciones, no importa cual es el contenido
de la lista. ¡Inclusive si la lista ya viene ordenada, la cantidad de comparaciones
sigue igual!

Ahora vamos a ver un algoritmo distinto, cuyo tiempo de ejecución (en cantidad de pasos)
depende del contenido de la lista, por más que fijemos su tamaño.

## Gnome sort (o "ordenamiento del duende del jardín") ##

La idea de este algoritmo es imaginarse un duende del jardín
ordenando macetas. Los únicos intercambios que puede hacer son dos
macetas una al lado de la otra.

El duende empieza por el principio de la lista y termina cuando
llega al final:

* si está mirando al primer elemento de la lista ($a_0$),
  o si el elemento que mira es mayor o igual al
  elemento que está justo a la izquierda ($a_i >= a_{i-1}$),
  entonces hace un paso a la derecha
* si el elemento que está mirando es menor al que está justo
  a la izquierda ($a_i < a_{i-1}$), entonces los intercambia,
  y hace un paso a la izquierda.

El duende repite este proceso hasta que llegue al final de la lista.

<div class="papel">
Escribir el algoritmo del gnome sort. Observar que, contrariamente
a selection sort, no sabemos *a priori* cuantas repeticiones va
a tener el bucle de este algoritmo. Pensar más bien en cuál es la
condición de repetición del bucle.
</div>

<div class="compu">
Implementar el algoritmo en una función `void selection_sort(int a[], const int size)`,
y agregarle llamadas a `color_ord` para mostrar el estado de la lista después
de cada intercambio.
</div>

## Escritura de archivos ##

Ahora queremos escribir en un archivo los sucesivos estados de
una lista. Vamos a tener que usar funciones de apertura, escritura
y cierre de archivos. La función de escritura, `fprintf`, es muy
parecida a `printf` pero toma como parámetro un valor que indica
el archivo dentro de cual vamos a escribir.

<div class="info">
Referencias útiles:

* [Wikilibro Programación en C Manejo de archivos](http://es.wikibooks.org/wiki/Programaci%C3%B3n_en_C/Manejo_de_archivos),
  en particular los ejemplos de `fclose` y `fprintf`.
</div>

<div class="compu">
Modificar el programa para que escriba, línea por línea, en un archivo
"ordenamiento.txt", los sucesivos estados de la lista.
</div>

## Generación de imágenes PGM ##

Con un poco de cuidado podemos escribir un archivo en el
[formato Netpbm](http://en.wikipedia.org/wiki/Netpbm_format), que es una
familia de formatos de imágenes sencillo. Si usamos el formato PGM
(Portable GrayMap) podemos visualizar las listas como líneas de píxeles
grises.

Un ejemplo de archivo .pgm:

~~~
P2
24 7
15
0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
0  3  3  3  3  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15 15 15 15  0
0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0 15  0
0  3  3  3  0  0  0  7  7  7  0  0  0 11 11 11  0  0  0 15 15 15 15  0
0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0  0  0
0  3  0  0  0  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15  0  0  0  0
0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
~~~

Viéndolo con zoom en el software Gthumb:

![ejemplo PGM](http://hub.darcs.net/gh/prog1/raw-file/contenidos/pgm.png)\ 

Si detallamos el formato de este archivo:

* la primera línea "P2" es obligatoria e indica el tipo de archivo.
* la segunda línea indica la cantidad de **columnas** y **líneas** de la imagen
* la tercera línea indica el valor máximo que se va a usar.
* luego viene la imagen, indicada píxel por píxel

Cada píxel es especificado por un valor entre 0 y el valor máximo indicado
en la tercera línea. Los valores intermedios corresponden a distintas sombras
de gris, de la más oscura a la más clara.

<div class="papel">
Si $n$ es el tamaño de las listas que ordenamos,
¿cuántas líneas va a tener la imagen generada por nuestro programa?

¿Sabemos decir antes cuántas líneas va a tener la imagen? ¿En qué casos?
</div>

<div class="compu">
Implementar la generación de imagenes PGM para representar el ordenamiento de listas
con selection sort.
</div>

Ayuda: para escribir una línea al archivo PGM pueden usar la función:

~~~C
void print_line_pgm(const int a[], const int size, FILE * f ){
    int k;
    for(k=0; k<size; k++){   
       fprintf(f,"%2d ", a[k]);
       if (k<size-1) fprintf(f," ",a[k]);
    }
    fprintf(f,"\n");
}
~~~

Probar aumentando el valor de la macro SIZE a 30, 50, etc.

![selection sort](http://hub.darcs.net/gh/prog1/raw-file/contenidos/selectionsort.png)\ 


