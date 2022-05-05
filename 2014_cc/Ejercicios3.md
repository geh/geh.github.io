# Ejecicios Unidad 3

1. (Sipser 8.1) Mostrar que para cualquier función $f$ tal que $f(n) \geq n$,
   la clase de complejidad **SPACE**$(f(n))$ es la misma, que la definamos
   con máquinas de Turing:
   * de una sola cinta, o
   * de dos con cintas con cinta de entrada en lectura sola

2. Mostrar que $A \leq_P B$ y $B \in$ **PSPACE** implica $A \in$ **PSPACE**.

3. Mostrar que si $A \leq_L B$ y $B \leq_L C$ entonces $A \leq_L C$.

4. (Sipser 8.18) Sea B el lenguaje de las paréntesis y corchetes bien anidados.
   Por ejemplo, `([()()]()[])` es de B pero `([)]` no lo es.
   Mostrar que B pertenece a **L**.

5. (Sipser 8.23) Sea UCYCLE ==
   {(G) | G es un grafo no dirigido que tiene un ciclo simple}.
   Mostrar que UCYCLE pertenece a **L**."

6. Leer en Sipser, páginas 313-320, las secciones Winning Strategies for Games
   y Generalized Geography.
   Consideremos el lenguaje uGG, el juego Generalized Geography en grafos
   no dirigidos (o simétricos).
   ¿Qué tendríamos que modificar en la demostración del Teorema 8.14 si
   quisieramos demostrar que uGG es PSPACE-completo?
   ¿Cuál es su complejidad? (Buscarlo en el internet.)
