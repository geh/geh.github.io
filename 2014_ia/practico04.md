# Práctico 4: juegos

Fecha de entrega: martes 14 de octubre

## Lectura preliminar

Apuntes de [inteligencia artificial de la UPC](http://www.lsi.upc.edu/~bejar/ia/teoria.html):

* [Juego](http://www.lsi.upc.edu/~bejar/ia/transpas/teoria/2-BH4-juegos.pdf)

Libro [Artificial Intelligence: a Modern Approach. Capítulo 5](http://aima.cs.berkeley.edu/).

## Ejercicios

1. Implementar el algoritmo MINIMAX para el juego "3 en rayo".

2. Implementarlo para el juego "Dualidad" siguiente:

    * el juego ocurre en un tablero 4 por 4 entre dos jugadores.
    * una casilla puede estar en tres estados posibles:
        1. estar vacía
        2. tener un círculo vacío
        3. tener un círculo lleno
    * los únicos movimientos posibles de los jugadores consisten
      en dibujar un círculo (vacío) o llenar un círculo ya dibujado
    * el jugador que completa una línea (vertical, horizontal o diagonal) de cuatro
      círculos de mismo tipo gana la partida.
