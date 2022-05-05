# Ejecicios Unidad 4

1. Mostrar que **PP** $\subseteq$ **PSPACE**.

2. (Arora Barak 7.1)

   > Show that one can efficiently simulate choosing a random number from 1 to N using
   > coin tosses. That is, show that for every N and δ > 0 there is a probabilistic algorithm
   > A running in poly(log N $\times$ log(1/δ))-time with output in {1, ... , N, ?} such that
   > (1) conditioned on not outputting ?, A’s output is uniformly distributed in [N] and
   > (2) the probability that A outputs ? is at most δ.

3. (Abhijit Das)
   Sea **RL** la versión de la clase **RP** pero con cota espacial
   logarítmica. Sea UPATH la versión no dirigida de PATH
   (ie, el problema de la conectividad en un grafo no dirigido).
   Un *camino aleatorio* en un grafo $G$ es una secuencia de pasos
   tal que en cada paso se elige un vecino al azar.
   En 1979, Aleliunas et al. mostraron que
   un camino aleatorio partiendo de $s$ y de
   $8\cdot \left|V(G)\right| \cdot \left|E(G)\right|$ (ie
   $O(m^3)$ con $m=\left|V(G)\right|$) pasos visita $t$ con
   probabilidad $\geq 1/2$. Describir un algoritmo en **RL**
   para UPATH usando ese resultado.


4. Erratum de la Unidad 3:
   (Sipser 8.23) Sea UCYCLE ==
   {(G) | G es un grafo no dirigido que tiene un ciclo simple}.
   Mostrar que UCYCLE pertenece a **L**."
   Indicación de Goldreich (en el libro *Computational complexity: a Conceptual Perspective*):

   > Consider a scanning of the graph that proceeds as follows. Upon entering a vertex
   > $v$ via the $i$th edge incident at it, we exit this vertex using its $i + 1$st edge if $v$ has degree at
   > least $i + 1$ and exit via the first edge otherwise. Note that when started at any vertex of any
   > tree, this scanning performs a DFS. On the other hand, for every cyclic graph there exists
   > a vertex $v$ and an edge $e$ incident to $v$ such that if this scanning is started by traversing the
   > edge $e$ from $v$ then it returns to $v$ via an edge different from $e$. 
