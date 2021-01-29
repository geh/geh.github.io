# Ejecicios Unidad 2

Fecha de entrega: 27 de noviembre.


1. Mostrar que **NP** $\subseteq$ **PSPACE**.

2. Sea **coC** la clase de los lenguajes cuyos complemento está
   en **C**. 
       * Mostrar que **P** $\subseteq$ **coNP**.
       * Mostrar que **P**=**NP** implica **NP**=**coNP**.

3. El problema de la autopista con peaje (turnpike problem) es el siguiente:
   "dadas n(n − 1) / 2 distancias entre pares de puntos,
   ¿les corresponde alguna configuración de n puntos en una línea?".
    * Definir un lenguaje que corresponde a ese problema.
    * Mostrar que ese lenguaje está en **NP**.

4. (Sipser 7.21) $DOUBLESAT= \{ \lfloor \varphi \rfloor \mid \varphi$ tiene
   al menos dos asignaciones satisfactoras $\}$
   Mostrar que DOUBLESAT es **NP** completo (usando reducciones de Karp en
   tiempo polinomial).

## Mas ejercicios

1. Mostrar que **P** ⊆ **NP** ∩ **coNP** (Arora Barak def. 2.19)
1. Mostrar que **P**=**NP** implica **NP**=**coNP**.
1. Suponer que $L_1,L_2 \in$ **P**.
   ¿Qué tal $L_1 \cup L_2$? ¿$L_1 \cap L_2$?
1. (Arora Barak 2.10) Mismas preguntas con **NP**.
1. Describir un algoritmo corriendo en tiempo polinomial para
   convertir una representación de grafo por matriz de adyacencia
   en lista de adyacencia. Describir un algoritmo que hace
   la conversión en la otra dirección.
1. (Arora Barak 1.14) Mostrar que los lenguajes siguientes
   (de grafos no dirigidos) están en **P**
    1. CONNECTED
    1. TRIANGLEFREE
    1. TREE
1. Monstrar que SAT $\leq_q$ CLIQUE
   adaptando la demostración del teorema 2.15 de Arora y Barak.
1. Mostrar:
   Sea $G=(E,V)$ es un grafo. $S$ es un conjunto independiente
   (independent set) ssi $V \setminus S$ es una cobertura de $G$ (vertex cover).
1. Usando 8., mostrar que VERTEXCOVER $\leq_p$ INDSET y
   INDSET $\leq_p$ VERTEXCOVER.

## Lecturas recomendadas

* Sipser, thm. 7.24, 7.25, 7.32, 7.44, 7.56
* Aaronson: [prueba del teorema de Cook-Levin sin usar MT indiferente (en sección 5)](https://stellar.mit.edu/S/course/6/sp12/6.045/courseMaterial/topics/topic1/lectureNotes/lec9/lec9.pdf)
* Karp "Reducibility Among Combinatorial Problems", 1972
* Aloupis, Demaine y Guo "Classic Nintendo Games are (NP-)Hard", 2012

