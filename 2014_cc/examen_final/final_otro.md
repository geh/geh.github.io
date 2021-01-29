# Kolmogorov complexity #

6.22) Show that the function K(x) is not a computable function.
6.23) Show that the set of incompressible strings is undecidable.

1. (Sipser 8.20) Sea $MULT$ = $\{a\#b\#cl | a, b, c$ son enteros naturales
   binarios y $a * b = c \}$.
   Demostrar que $MULT\in L$.

6. (Sipser 5.15)
   Considerar el problema de determinar si una M.T. $M$ con una
   palabra de entrada $w$, nunca intenta mover su cabezal a la izquierda
   en algun momento de su ejecución sobre $w$.
      1. Formular el problema como un lenguaje.
      2. Demostrar que es decidible.


3. (Sipser 7.9) A triangle in an undirected graph is a 3 -clique.
    Show that $TRIANGLE \in P$, where
    $TRIANGLE$ = {(G) | G contains a triangle}.

1. (Sipser 7.20)
   Consideramos dos lenguajes hechos con palabras de la forma
   $<G,a,b,k>$, con $G$ un grafo no dirigido, $a,b \in G$
   y $k$ un entero natural.
   Sea $SPATH$ el lenguaje de los grafos que tienen un camino simple
   de $a$ a $b$ de tamaño $\leq k$. 
   Sea $GPATH$ el lenguaje de los grafos que tienen un camino simple
   de $a$ a $b$ de tamaño $\geq k$.
       1. Demostrar que $SPATH \in P$.
       2. Demostrar que $LPATH$ es completo para NP.
          Para eso suponer la NP-completitud de
          $UHAMPATH$, el lenguaje de los grafos no dirigidos
          que tienen un camino Hamiltoniano (camino que visite
          cada nodo exactamente una vez).

11. (Sipser 7.19) Es creencia común que PATH no es completo para NP.
       1. Explicar en qué intuición descansa esta creencia.
          (Pensar en qué hacer para demostrar que es completo
          para NP).
       2. Demostrar que si PATH no es completo para NP, entonces
          $P \neq NP$.


5. Demostrar que si el lenguaje $L$ es decidible, entonces
   $L' = \{ w' | w'$ es el inverso de $w\in L\}$
   lo es también.

10. (Sipser 7.17) Mostrar que si $P = NP$ entonces todo lenguaje
    $A \in P$ salvo $\emptyset$ y $\Sigma^*$, es completo para NP.


