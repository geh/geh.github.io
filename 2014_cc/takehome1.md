% Complejidad Computacional
  Take Home 1

A entregar durante la clase del jueves 11 de octubre 2012.

1. ¿Porqué podemos decir que no cambia nada tener máquinas de Turing
   con $1$ o $n$ registros internos con respeto al poder computacional?

1. Sea $L_1, L_2 \subseteq\{0,1\}$. Llamamos $L_1 \circ L_2$ el lenguaje
   formado por la concatenación de strings de $L_1$ y $L_2$:
   $$L_1 \circ L_2 = \{ x \circ y \mid x \in L_1 , y \in L_2 \}$$
   Demostrar o infirmar las siguientes proposiciones:
       1. Si $L_1$ y $L_2$ están en **NP** entonces $L_1 \circ L_2$ también
       1. Si $L_1$ y $L_2$ están en **PSPACE** entonces $L_1 \circ L_2$ también
       1. Si $L_1 \circ L_2$ está en **P**, entonces $L_1$ y $L_2$ también

1. El problema de la autopista con peaje (turnpike problem) es el siguiente:
   "dadas n(n − 1) / 2 distancias entre pares de puntos,
   ¿les corresponde alguna configuración de n puntos en una línea?".
    * Definir un lenguaje que corresponde a ese problema.
    * Mostrar que ese lenguaje está en **NP**.

1. (Sipser 7.21) $DOUBLESAT= \{ \lfloor \varphi \rfloor \mid \varphi$ tiene
   al menos dos asignaciones satisfactoras $\}$
   Mostrar que DOUBLESAT es **NP** completo.

1. En la demostración del teorema de Cook-Levin del libro de Arora y Barak,
   se construye una fórmula CNF cuyas variables codifican una serie de
   instantaneos y que es verdadera si, y solamente si, esos instantaneos
   representan una computación valida.
   Una parte de esa formula chequea cada instantaneo $z$ con respeto al
   último instantaneo cuando el cabezal de escritura de la máquina estaba en la misma
   posición que estaba para $z$. ¿Qué debe ser chequeado y porqué?
   ¿Cómo se determina con respeto a cual instantaneo hay que hacer el
   chequeo?


