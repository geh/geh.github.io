# No interesante

1. Definimos la clase de complejidad siguiente.
   **NLIN** : $L \in$ **NLIN** si cumple con las mismas condiciones que
   $L \in$ **NP** excepto que el certificado de un input $x$ es de
   de tamaño lineal en funcion de $x$ (ie $u\in\{0,1\}^{c\cdot \left|x\right|}$
   con $c\in\mathbb{N}$). Mostrar que **NLIN**$\neq$**NP**.

1. (Arora Barak Theorem 2.16) Mostrar que 0/1PROG es **NP** completo.

1. (Arora Barak 2.30) (Teorema de Berman 1978) Un lenguaje es *unario* si
   es de la forma $\{ 1^k \mid k \in C \subseteq \mathbb{N} \}$. Mostrar
   que si existe un lenguaje unario **NP** completo, entonces **P**=**NP**.
   Hint: si existe una reducción en tiempo $n^c$ desde $3SAT$ hasta un
   lenguaje unario $L$, entonces esa reducción mapea instancias de $3SAT$
   de tamaño $n$ a strings de la forma $1^i$ con $i \leq n^c$. Usar esto
   para lograr un algoritmo polinomial para SAT.

1. (Arora y Barak 4.6)
   Asumimos que definimos la **NP** completitud usando reducciones logspace
   en lugar de reducciones en tiempo polinomial. Mostrar (usando la prueba
   del teorema de Cook Levin) que SAT y 3SAT sigue siendo **NP** completo
   bajo esa nueva definición.

1. (Arora y Barak 4.7)
   Mostrar que, en la definición de **NL** con certificados, si permitimos
   a la verificadora de mover su cabezal en ambas direcciones, entonces
   la clase definida es **NP**.

1. 2SAT in NL

1. (Sipser 8.18) Sea $B$ el lenguaje de las parentesis y llaves correctamente
   anidadas. Por ejemplo "([()()]()[])" está en $B$ pero $([)]$ no lo está.
   Mostrar que $B\in$ **L**.

1. (Sipser 8.17) Sea $A$ el lenguaje de las parentesis correctamente
   anidadas. Por ejemplo "(())" y "(()(()))()" están en $A$ pero $)($
   no lo está. Mostrar que $A \in$ **L**.


