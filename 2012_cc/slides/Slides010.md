% Complejidad Computacional
  Semana 1: máquinas de Turing y **P**

# Organización de la materia

* horarios: martes 10-12hs, jueves 10-13hs, aula 24
* evaluación: 2 take-homes
* sitio web <http://tinyurl.com/ccfamaf12>

# Bibliografía

* 'Computational Complexity: A Modern Approach', Arora y Barak, 2007
    ([Draft](http://www.cs.princeton.edu/theory/index.php/Compbook/Draft))
* 'Introduction To The Theory Of Computation', Sipser, 1996.
* 'The annotated Turing', Petzold, 2008.

# Preámbulo

Se recomienda conocer un poco acerca de máquinas de Turing,
y el resultado de la existencia de una máquina universal de Turing.

Referirse a [CC:AMA, capítulo 1](http://www.cs.princeton.edu/theory/complexity/modelchap.pdf)
en particular secciones 1.2 a 1.4.

# Computabilidad

* ¿cuales son las cosas que se pueden *calcular*?
* "f es computable si existe un procedimiento efectivo p tal que
  p(x) = f(x) para todo x"
* "$x \in \mathbb{R}$ es computable si existe un procedimiento efectivo
  p tal que p genera las decimales de $x$"
* "procedimiento efectivo": algo que se puede hacer en el mundo real

# 1936: la propuesta de Turing

Turing propuso de manera convincente un modelo de computación
que llamó "computing machines".

Su forma de proponerlo era:

* describir el comportamiento y los límites de un calculador humano
* introducir la "computing machine" cómo objeto matemático
* demostrar que los dos tienen el mismo poder computacional

# Resultados importantes

* las máquinas de Turing tienen una descripción finita y son enumerables
* la gran mayoría de los números reales es incomputable
* existe una máquina de Turing que puede leer la descripción de cualquier
  máquina de Turing, y imitar su comportamiento (máquina universal)

# Años 1960: complejidad computacional

* ¿cuales son las cosas que se pueden calcular *de manera eficaz*?
* de manera eficaz = con recursos limitados (tiempo, memoria,...)
* ¿Cual es la dificultad *intrinseca* de un problema?
* ¿Hay problemas computacionalmente más difíciles que otros?

# Formas inadecuadas de encarar el problema

Ya sabemos medir las cosas siguientes en función de la longitud
de la entrada:

* hacer benchmarks de programas
* calcular el tiempo de corrida de un algoritmo que calcula $f$
  en función de la longitud de la entrada
    * ¿cómo saber si no hay mejor algoritmo para calcular $f$?

# Como los vamos a resolver

* sí nos importa la relación entre el *tamaño de la entrada* (para cualquiera
  entrada posible) y los recursos usados para llevar a cabo un cálculo
* queremos que no importe el lenguaje de programación usado para computar
* sin embargo queremos medir o clasificar la complejidad de manera informativa
* necesitamos un modelo que:
    * represente los cálculos que se hacen el el mundo real
    * permite medir el costo de un cálculo
        * Máquinas de Turing

# Lenguages y funciones

* Palabra (o string): $x \in \{0,1\}^*$
* Lenguage: $L \subseteq \{0,1\}*$ (conjunto de palabras)
* Función booleana: $f: \{0,1\}^* \mapsto \{0,1\}$
* $f$ es la *función característica* de $L_f$ si:
  <br />$L_f = \{ x ~ \mid ~ f(x) =1 , x \in \{0,1\}^* \}$
* "Computar $f$" = dado un $x\in\{0,1\}^*$, computar $f(x)$
* "Computar $f$" = "Decidir $L_f$"
* $f$ es un *problema de decisión*

# Unos lenguajes sobre $\{0,1\}^*$

* PAL = $\{ x \mid x$ es capicúa $\}$
* $\lfloor x \rfloor$ = representación de $x$ como string binaria
* EVEN = $\{ \lfloor x \rfloor \mid x$ es par $\}$
* SMALLER = $\{ \lfloor (x,y) \rfloor  \mid x \leq y \}$
* PRIMES = $\{ \lfloor x \rfloor  \mid x$ es primo $\}$
* HAM = $\{ \lfloor G \rfloor  \mid G$ es hamiltoniano $\}$
<!--
* SUBSETSUM = $\{ (X,x) \mid \exists S\subseteq X, \Sigma S = x \}$
  (con X conjunto de enteros, x entero)
* INDSET =
  <br />$\{(G,k)\mid\exists S\subseteq V(G),|S|\geq k,\forall uv\in S,uv\notin E(G)\}$
  (con $G$ un grafo, $V(G)$ los puntos de $G$, $E(G)$ los ejes de G)
-->


# Los problemas de decisión *no* son...

...problemas de función (encontrar el $x$ tal que...).

Sin embargo, los problemas de función suelen tener su
equivalente en problemas de decisión:

* travelling salesman
    * función: dada una lista de ciudades con sus distancias,
      encontrar el recorrido exaustivo más corto
    * decisión: dada una lista de ciudades con sus distancias y
      una longitud $l$, existe un recorrido de longitud inferior a $l$?

# Nuestras Máquinas de Turing

Tienen:

* $k$ cintas (1 para el input, lo demas para cálculos
  intermedios) infinitas, siendo una successión de celdas
* un cabezal por cinta, capaz de leer, escribir y moverse
* un conjunto finito de estados internos
* una funcion de transición que describe su comportamiento

# 

<img src="mttext.png" />

# MT: un paso

En función de su estado interno activo y de los símbolos leidos
por los cabezales en todas las cintas, y según la función de transición,
un paso de una MT consiste en:

* escribir en las cintas
* mover cada una de sus cabezales de 0 o 1 paso (=, ←, →)
* cambiar su estado activo 

#
<div class="thm">
Una MT es un tuple $(\Gamma,Q,\delta)$ con:

* un alfabeto finito $\Gamma \supseteq \{ \square, \triangleright, 0, 1 \}$
  <br />($\square$ "blank", $\triangleright$ "start")
* un conjunto finito de estados $Q \supseteq \{q_{start} , q_{halt}\}$
* una función de transición
  <br />$\delta : Q \times \Gamma^{k-1} \mapsto Q \times \Gamma^{k-1} \times \{←,=,→\}^k$
  <br />$\delta(q_{halt},\_)$ no está definido
* Configuración inicial:
    * primera cinta:$\triangleright INPUT ~ \square ~ \square ~ \square ~ ⋅ ~ ⋅$
    * las demas:$\triangleright \square ~ \square ~ \square ~ ⋅ ~ ⋅$
    * estado activo: $q_{start}$
</div>

# MT que computan

<div class="thm">
Una MT *computa* una función $f:\{0,1\}^*\mapsto\{0,1\}$ si, dada una
configuración inicial con entrada $x \in \{0,1\}^*$, se detiene después
de un número finito de pasos con $f(x)$ escrito en la última cinta.

Una MT computa $f$ en tiempo $T(n)$ si su computación en cada entrada $x$
necesita *cómo máximo* $T(|x|)$ pasos.
</div>

# Observación

Nos interesa la cantidad de pasos hechos por una MT, pero el número de
estados internos no importa.

Resultado: una MT puede simular $n$ "registros" (estados activos) tomando valores
en los conjuntos $R_1, R_2, ..., R_n$ con un registro tomando valores en $R_1 \times R_2 \times ... \times R_n$.

# MT indiferente

<div class="thm">
Una MT es *indiferente* (eng. *oblivious*) si los movimientos de sus cabezales
sólo dependen de la longitud de sus inputs y no de los bits.
</div>

Es decir, para todos los inputs de longitud $n$, existe une función
$pos: \mathbb{N} \mapsto \mathbb{N}^k$ tal que $pos(x)$ es
la posición de los $k$ cabezales en el paso $x$.

# Substitución de MT

<div class="thm">
Para toda $f : \{0,1\}^* \mapsto \{0,1\}$
y $T: \mathbb{N} \mapsto \mathbb {N}$ constructible en tiempo,
si $f$ es computable
por una MT con $k$ cintas y un alfabeto $\Gamma$ en tiempo $T(n)$,
entonces es computable por:

1. una MT con alfabeto $\{0,1,\square,\triangleright\}$
   en tiempo $4log|\Gamma|(T(n))$
2. una MT con 2 cintas en tiempo $O(T(n)^2)$
3. una MT indiferente en tiempo $O(T(n)^2)$
</div>

* 3. se puede mejorar en $O(T(n).log(T(n)))$ (Pippenger y Fischer, 1979).

# Más resultados similares

Más resultados de cambio de tipo de MT con deceleración polinomial:

* pasar de cintas con $k$ dimensiones a cintas con $1$ dimensión
* pasar de memoria en acceso random a  memoria en acceso secuencial
* pasar de $n$ estadios activos (= registros) a $1$ (constante, sólo aumenta el número de estados)

# Linear speed-up theorem (Hartmanis y Stearns, 1965)

<div class="thm">
Si $f$ es computable por una MT $M$ en tiempo $T(n)$, entonces
para cualquiera constante $c \geq 1$, $f$ es computable por una MT $M'$ en
tiempo $T(n)/c$.
</div>

* Demo:
    * $M'$ maneja "word length" más grande que $M$
    * $M'$ tiene cómo alfabeto $\Gamma \cup \Gamma^m$
    * paso 1: comprimir cinta input en cinta nueva
    * paso 2: simular $m$ pasos de $M$ en $6$ pasos de $M'$
      (4 para leer, 2 para escribir)
    * eligir $m = 6c$


# Clase de complejidad **TIME**

<div class="thm">
Sea $T: \mathbb{N} \mapsto \mathbb{N}$. Un lenguaje $L$ es en
**TIME**(T(n)) si existe una MT que corre en tiempo $c⋅T(n)$
(para un $c > 0$) y decide $L$.
</div>

<!--

* El factor multiplicativo está por abstraerse de diferencias de
  alfabetos y por el speed-up theorem.
* Ejemplo de caso problemático si no precisamos que tipo de MT usamos:
  $L = \{0^k 1^k \mid k \geq 0 \}$:
    * con 2 cintas, decidible en O(n)
    * con 1 cinta, decidible en O(n.log(n)))
-->


# **P**

Una manera formal de definir los calculos rápidos:

<div class="thm">
**P** = $\bigcup_{p ~ polinomio}$ **TIME**$(p(n))$ = $\bigcup_{c \geq 1}$ **TIME**$(n^c)$
</div>

La clase es robusta si cambiamos el tipo de MT que usamos.

Doblar el tamaño del input sólo necesita $k$ veces más tiempo (con $k$ constante):

* inputs de longitud $n$ necesitan tiempo $n^c$
* inputs de longitud $2n$ necesitan $(2n)^c = (2^c)n^c$

# Con **P** (y arriba), no importa (tanto) el modelo

Cuando vamos a describir algoritmos para **P**, no vamos a describir MMTT hasta
los últimos detalles.

Cuando tenemos un algoritmo en pseudocódigo
cuya complejidad podemos caracterizar, podemos decir que
tenemos una MT que implementa ese mismo algoritmo, con
una deceleración polinomial.

# Codificación

* Tenemos que medir el tamaño de los datos manejados por las MMTT
  (y/o nuestros algorítmos) en *bits*.
* Tiene un impacto como representamos los símbolos,
  números, grafos, conjuntos, mátrices, etc.
* En particular, escribir un número en base binaria es exponencialmente
  más sucinto que en base unaria. Puede ser que un algorítmo para
  $L_{unario}$ corra en tiempo polinomial pero su equivalente para $L_{binario}$
  en tiempo exponencial!
* Buena noticia, operaciones usuales (eg $+$, $-$, $.$, $/$)
  se pueden hacer en tiempo polinomial con representación binaria

# Críticas de "**P** = problemas fáciles"

* es demasiado permisiva
* es demasiado estricta

# ¿Dónde está **P**?

* ¿todos los lenguajes están en **P**?
* ¿todos los lenguajes decidibles están en **P**?


<!--
# Funciones constructibles en tiempo
-->


# Problemas arbitrariamente difíciles

<div class="thm">
Para toda función $T: \mathbb{N} \mapsto \mathbb{N}$ constructible en tiempo,
existe un lenguaje $L_T$ decidible que *no* puede ser decidido por una MT corriendo en tiempo $T(n)$. 
</div>

# Demostración

$L_T = \{ \lfloor M \rfloor \mid M$ es una MT y $M(\lfloor M \rfloor) =1$
en $T(|\lfloor M \rfloor|)$ pasos $\}$

* $L_T$ es decidible (usar MT universal)
* $L_T$ *no* se puede decidir en menos de $T(n)$ pasos
    * supongamos $\exists$ N que decide $L_T$ en $\leq T(n)$ pasos.
    * entonces $\exists$ N' (definida gracias a N) tal que:
        * si input $\in L_T$, corre siempre
        * si input $\notin L_T$, se detiene y imprime $1$ (en $\leq T(n)$ pasos)
        * ¿Qué pasa con $N'(\lfloor N' \rfloor)$?

# Time hierarchy theorem (Hartmanis y Stearns, 1965)

Generalización del resultado previo:

<div class="thm">
Si f,g son funciones constructibles en tiempo tales que $f(n).log(f(n)) = o(g(n))$, entonces

**TIME**$(f(n))$ $\subset \neq$ **TIME**$(g(n))$
</div>

En particular: **TIME**($n^c$) $\subset \neq$ **TIME**($n^d$) con $c < d$.

# Aplicación

<div class="thm">
**EXPTIME** = $\bigcup_{c \geq 0}$**TIME**$(2^{n^c})$.
</div>

Consecuencia del time hierarchy theorem:
<div class="thm">
**P** $\subset$ **EXPTIME**
</div>

# Conclusiones

* Noción de complejidad intrinseca de un lenguaje
* Definir una clase de complejidad en tiempo de corrida de una MT,
  necesita no ser demasiado preciso ni demasiado impreciso
* La clase **P** es robusta:
  nos podemos olvidar del alfabeto usado, del número de cintas,
  de si tenemos acceso indexado a la memoria, etc.
* Lo que se viene:
    * clase **NP**
    * reducciones polinomiales
    * completitud
    * teorema de Cook-Levin

# Referencias

* Hartmanis y Stearns, "On the computational complexity of algorithms", 1965
* Pippenger y Fischer, "Relations among complexity measures", 1979
* Scott Aaronson,
  [Automata, Computability and Complexity](https://stellar.mit.edu/S/course/6/sp12/6.045/materials.html)
* Dick Lipton
  [Programming Turing Machines is Hard](http://rjlipton.wordpress.com/2010/01/27/programming-turing-machines-is-hard/)
