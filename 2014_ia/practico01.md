# Práctico 1: Python

Objetivos:

* configurar un entorno para programar en Python
* subir su código a un sitio de alojamiento de repositorios
* escribir un programa python de complejidad mediana
* generar grafos simples con el módulo matplotlib

Fecha de entrega: 8 de septiembre.

Modo de entrega: repositorio en [hub.darcs.net](http://hub.darcs.net).

## Instalación y configuración

Se recomienda usar una distribución Linux de tipo Debian o Ubuntu.
Si no saben qué usar, usar Ubuntu 14.04.

Van a necesitar instalar paquetes:

    sudo apt-get install ipython darcs msmtp-mta meld

## Gestion de versiones

Para rendir los practicos y proyectos, van a usar [Darcs](http://darcs.net),
un sistema de gestion de versiones.
La idea es alojar un repositorio personal con acceso privado
y darme acceso (en lectura por lo menos).
Eso permitira no usar mails para rendir los ejercicios y no perderse el código
que van a hacer.

### Creacion de cuenta en hub.darcs.net y configuracion ssh

Para alojar su código en hub.darcs.net,
[crearse una cuenta](http://hub.darcs.net/register)
(el ultimo campo tiene que ser llenado con "darcs").

Para comunicarse con ese sitio, es necesario generar un par de claves
publicas/privadas para ssh:

    ssh-keygen

Apretar *enter* para cada pregunta. Se generan un par de archivos
`~/.ssh/id_rsa.pub` (clave pública) y `~/.ssh/id_rsa` (clave privada).

Copiar todo el contenido de `~/.ssh/id_rsa.pub` en el campo *pubkeys* de
[la pagina de configuracion del sitio](http://hub.darcs.net/settings).
Podrán comunicarse con el sitio usando Darcs a partir de cualquier computadora
que tenga su clave privada en `~/.ssh/id_rsa`.

Se pueden usar esos archivos en varias computadoras
para acceder al sitio. Se puede volver a generar claves nuevas si las
perden, solo habrá que poner al día la configuración en el sitio.

### Crear un repositorio en hub.darcs.net

1. Hacer click en [*new repo*](http://hub.darcs.net/init)
2. Llenar el campo *name*. Por favor usar el nombre "ia2014_APELLIDO".
3. Activar la opcion *private*.
4. Hacer click en *create repository*. Se crea el repositorio.
5. Hacer click en *repo settings*.
6. En el campo *add members* agregar el login gh (el profesor) y confirmar.
7. Desde la pagina del repositorio, copiar la URL indicada (tiene la forma `LOGIN@hub.darcs.net:ia2014_APELLIDO`)
8. En linea de comando hacer:

    darcs clone LOGIN@hub.darcs.net:ia2014_APELLIDO

9. Entrar a la carpeta `ia2014_APELLIDO` y empezar a trabajar.

Leer [http://darcs.net/QuickStart](http://darcs.net/QuickStart) y las páginas
relacionadas para una introducción a Darcs.

## Python

Vamos a usar la version 2.7 de Python (existe la versión 3 del lenguaje
pero todavía muchas bibliotecas no están al día).

Recursos utiles:

* [Tutorial Python 2.7](https://docs.python.org/2/tutorial/index.html)
* [Tutorial con ejemplos de codigo y output](http://nbviewer.ipython.org/github/ehmatthes/intro_programming/blob/master/notebooks/syllabus.ipynb)
* [Visualizador de ejecución de código Python](http://pythontutor.com/visualize.html)

Python usa la indentacín para definir bloques de código. La convención
que se usa es **indentar con 4 espacios**, y no usar tabulaciones. Pueden
configurar su editor de texto para mostrar la tabulaciones. Por ejemplo en Geany,
activar la opción *Edit* > *Preferences* > *Editor* > *Display* > *Show white spaces*.

## Lectura Preliminar

1. Leer y probar en el intérprete `ipython` las secciones:
    * [Hello World](http://nbviewer.ipython.org/urls/raw.github.com/ehmatthes/intro_programming/master/notebooks/hello_world.ipynb)
    * [Variables, Strings and Numbers](http://nbviewer.ipython.org/urls/raw.github.com/ehmatthes/intro_programming/master/notebooks/var_string_num.ipynb)
    * [Lists and Tuples](http://nbviewer.ipython.org/urls/raw.github.com/ehmatthes/intro_programming/master/notebooks/lists_tuples.ipynb)
    * [Introducing Functions](http://nbviewer.ipython.org/urls/raw.github.com/ehmatthes/intro_programming/master/notebooks/introducing_functions.ipynb)
    * [If Statements](http://nbviewer.ipython.org/urls/raw.github.com/ehmatthes/intro_programming/master/notebooks/if_statements.ipynb) 


2. [En la interfaz de PythonTutor](http://pythontutor.com/visualize.html),
   ejecutar paso por paso los ejemplos de código (en **Examples to visualize**):

    * tokenize
    * for-else
    * Pointer Aliasing

3. Leer y probar en el intérprete `ipython` las secciones:

    * [While Loops and Input](http://nbviewer.ipython.org/github/ehmatthes/intro_programming/blob/master/notebooks/while_input.ipynb)
    * [Basic Terminal Apps](http://nbviewer.ipython.org/github/ehmatthes/intro_programming/blob/master/notebooks/terminal_apps.ipynb)

## Ejercicios

1. Implementar [nqueens.py](code/nqueens.py), el problema de las *n reinas*.
   Es una generalización del problema de las
   [las 8 reinas](https://en.wikipedia.org/wiki/Eight_queens_puzzle) para
   un tablero de tamaño indicado por el usuario.
   La idea no es nada más que usar un algoritmo exaustivo y sencillo
   para buscar una solución.

2. Definir una sección que se ejecuta solamente
   cuando se corre el script en linea de comando (es decir que
   no tenga que correr si el script está importado).
   Ver [la sección __main__ del tutorial](https://docs.python.org/2/library/__main__.html).
   Hacer que se pida el tamaño del tablero al usuario solamente si el script
   está ejecutado, y no importado.

3. Definir una función `benchmark(sizes)` que toma una lista de tamaños
   de tableros y genere un grafo de tiempo de corrida de la búsqueda
   de una solución.
   * usar el módulo [datetime](http://garmoncheg.blogspot.com.ar/2012/08/python-simple-recipe-to-measure-your.html)
     para medir el tiempo de corrida de algun pedazo de código.
   * usar el módulo [matplotlib](http://matplotlib.org/examples/pylab_examples/simple_plot.html)
     para generar el grafo y escribirlo en un archivo.
     (Se requiere instalar el paquete `python-matplotlib`).

4. **(Problema facultativo)** Implementar el problema de la [n+1
   reinas](http://www.chessvariants.org/problems.dir/9queens.html):
   para un tablero de tamaño `n`, buscar una solución con `n+1` reinas y `1` peón.
