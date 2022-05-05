% Complejidad Computacional
  Take Home 2

Para el jueves 1 de noviembre de 2012.

1. $M$ es una MT determinística con 1 cinta de trabajo corriendo en tiempo
   $t:\mathbb{N}\mapsto\mathbb{N}$.
   Dar una cota superior del tamaño del grafo de configuración de $M$ con input
   $x \in\{0,1\}^*$, $G_{M,x}$.

1. ¿Porqué una máquina de Turing determinística decidiendo un lenguaje $L$
   en espacio $s:\mathbb{N}\mapsto\mathbb{N}$
   tiene un tiempo de ejecución acodato?

1. Sea SUBSTRING el lenguaje siguiente:
   SUBSTRING = $\{ \lfloor \alpha , \beta \rfloor \mid$ $\beta$ es una
   substring de $\alpha\}$.
   Por ejemplo $\lfloor abracadabra, rac \rfloor$ pertenece a SUBSTRING,
   $\lfloor abracadabra, cadac \rfloor$ no pertenece a SUBSTRING.
   Mostrar que SUBSTRING $\in$ **L**.

1. (Sipser 8.11) Mostrar que si todo lenguaje **NP** difícil es
   tambien **PSPACE** difícil, entonces **PSPACE**=**NP**.

1. (Abhijit Das)
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

