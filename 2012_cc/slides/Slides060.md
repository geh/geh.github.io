% Complejidad Computacional
  Práctica 2

1. Definir un lenguaje que corresponde [al problema siguiente](http://blog.computationalcomplexity.org/2010/07/what-is-complexity-of-these-problems.html) y mostrar que
   está en **NP**:
   "We are going to put numbers into boxes. If x,y,z are in a box then it CANNOT
   be the case that x+y=z. If you put the numbers 1,2,...,n into boxes, what is
   the smallest number of boxes you will need?" 

1. Mostrar que $A \leq_P B$ y $B \in$ **PSPACE** implica $A \in$ **PSPACE**.

1. Dar 2 razones para qué los factores lineales no importan en la definición
   de la clase de complejidad **TIME**$(f(n))$
   (para $f:\mathbb{N}\mapsto\mathbb{N}$).

1. Mostrar que **P**=**coP**.

1. (Arora Barak 2.17)
   El problema EXACTLY ONE 3SAT consiste en tener una formula 3CNF $\varphi$
   y decidir si existe una asignación $u$ tal que cada cláusula de $\varphi$
   tiene exactemente un literal verdadero.
   Mostrar que Exactly One 3SAT es **NP** completo.
   Hint: reemplazar cada ocurrencia de un literal $v_i$ de una cláusula $C$
   por una variable nueva $z_{i,C}$ con claúsulas y variables adicionales
   tales que si $v_i$ es falsa, entonces $z_{i,C}$ tiene que ser falsa.

1. Dar una definición de **NEXP** con certificados tal que es equivalente
   a la definición con nondeterminismo visto en aula (demostrar la
   equivalencia).

1. Sea $f:\{0,1\}^* \mapsto \{0,1\}$.
   Decimos que una máquina de Turing $M$ computa $f$ con un caché hasta tamaño
   $m$ si para todo input $x$ de tamaño $\leq m$ (con $M\in\mathbb{N}$),
   la máquina computa $f(x)$ en un paso. Para todos los otros inputs,
   $M$ corre en tiempo $s:\mathbb{N}\mapsto\mathbb{N}$.
   ¿Qué decir del la cota temporal de $M$?

1. (Arora Barak 2.13) Una reducción $f$ desde un lenguaje en **NP** $L$
   a un lenguaje en **NP** $L'$ es parsimoniosa si el número de certificados de
   $x$ es igual al número de certificados de $f(x)$.
   Mostrar que la reducción de nuestra demo del teorema de Cook Levin
   puede ser modificada en una reducción parsimoniosa (hint: hacer
   que el instantaneo final de la verificadora sea único).

