% Complejidad computacional - Examen final
% 2015-08-08

# Máquinas de Turing #

1. (Sipser 3.7) Explicar porque lo siguiente **no** es una descripción
   aceptable de una máquina de Turing:

   $M_{mala}$ tiene como entrada un polinomial $p$ sobre las variables
   $x_1, ... , x_k$.
  
   1. Probar todas las configuraciones posibles de $x_1, ... x_k$ con valores enteros.
   2. Evaluar $p$ sobre todas esas configuraciones.
   3. Si cualquiera de esas configuraciones se evalua a $0$, *accept*, sino, *reject*.

# Lenguajes decidibles #

2. Demostrar que el lenguaje constituido de las palabras $<a,p>$ ,
   con $a$ una automata finito determinista y $p$
   tal que $a$ acepta $p$, es decidible.

3. ¿Con qué argumento no constructivo podemos decir que existen
   lenguajes no decidibles?

# Reducciones #

4. Un lenguaje unario es un lenguaje que es un
   subconjunto de $\{1\}^*$. Demostrar que existe un lenguaje
   unario indecidible.

5. (Sipser 5.13) Un *estado inútil* de una M.T. es un estado que
   la máquina nunca alcanza, para ninguna palabra de entrada.
   Considerar el problema de determinar, para una M.T. $M$,
   si tiene algun estado inútil.
       1. Formular el problema como un lenguaje.
       2. Demostrar que no es decidible. (Pista: reducir
          el problema de la aceptación de una palabra por una M.T.
          a este problema).
       
# Complejidad, P, NP #

6. Dar por lo menos 2 razones por cuales no importan los factores
   multiplicativos
   constantes cuando decimos que un lenguaje $L$ pertenece a
   un clase **TIME**$(f(x))$.

7. $PATH$ es el lenguaje de los triples $<G,a,b>$
   tal que $G$ es un grafo dirigido, $a,b \in G$ y existe un
   camino dirigido de $a$ a $b$. Mostrar que $PATH$ está en P.

8. (Sipser 7.11) Dos grafos $G$ y $H$ son *isómorfos* si los nodos
   de $G$ puden ser reordenados de tal forma que $G$ es idéntico a $H$.
   Sea ISO el lenguaje de los pares de grafos que son isómorfos.
   Mostrar que $ISO \in NP$.

9. (Sipser 7.14) Mostrar que P es cerrado bajo la operación $*$
   (repetición una cantidad finita de veces).
   (Pista:
   Con una entrada $y = y_1 ... y_n$ for $y_i \in \Sigma$,
   construir una table que indica para cada $i \leq j$ si
   la sub-palabra $y_i ... y_j \in A^*$ para toda $A \in P$.)

# Complejidad en espacio #

10. ¿Qué tipo de reducción usamos para definir la noción
    de completitud para NL, y porqué?

11. (Sipser 8.27) Sea $CONECTADO$ el lenguaje de los grafos
    dirigidos tales que cada par de puntos es conectado por
    un camino dirigido, en las dos direcciones.
    Mostrar que $CONECTADO$ es completo para NL.

# Aleatoriedad #

12. ¿Cuales son las diferencias (en sus definiciones y en
    sus aplicaciónes) entre una máquina de Turing
    aleatoria y una máquina nondeterminista?

13. ¿Porqué puede ser interesante descubrir que un problema
    pertenece a una clase aleatoria, cuando ya sabemos que pertenece a
    una clase "clasica"? ¿Cuál es el peligro, y como superarlo?

