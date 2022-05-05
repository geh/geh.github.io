# Práctico 2: búsqueda heurística

Fecha de entrega: martes 23 de septiembre

## Lectura preliminar

Apuntes de [inteligencia artificial de la UPC](http://www.lsi.upc.edu/~bejar/ia/teoria.html):

1. [Búsqueda](http://www.lsi.upc.edu/~bejar/ia/transpas/teoria/2-BH1-introduccion_busqueda.pdf)
2. [Búsqueda heurística](http://www.lsi.upc.edu/~bejar/ia/transpas/teoria/2-BH2-Busqueda_heuristica.pdf)

Libro [Artificial Intelligence: a Modern Approach. Capítulos 3 y 4](http://aima.cs.berkeley.edu/).

## Ejercicios

Implementar el [n puzzle](http://en.wikipedia.org/wiki/15_puzzle)
en un script Python.

Solamente cuando se ejecuta, el script pide al usuario la posición
inicial del rompecabezas o propone posiciones por defecto.

Implementar con búsquedas (filmina 20 del teórico 1.):

1. En anchura primera

   Probar con el tablero ``[[1,2,3],[4,5,6],[8,7,0]]``. El programa encuentra la solución?
   Probar con tableros cercanos de la posición exitosa.

2. En profundidad primera. Modificar `search` para que acepte, además del primer
   tablero, una función que se defina el tipo de búsqueda.

3. A* con 2 heurísticos distintos por lo menos. Facilitar la selección del tipo
   de búsqueda y de heurístico usando funciones como parametros.
   (Teórico 2., filminas 6-14).

4. Modificar las funciones para que el programa maneje tableros de tamaño 4 o más.

Ayuda:

* [Implementación incompleta para el 8-puzzle](code/npuzzle.py),
* La función `deepcopy` del módulo `copy` se usa para hacer copias
  profundas de objetos (para evitar problemas de aliasing).
* En un intérprete python leer `help(list)`.

