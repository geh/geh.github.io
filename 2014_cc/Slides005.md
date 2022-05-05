% Complejidad Computacional
  <br />Unidad 1: Modelos de cálculo

# Objetivos de la unidad

Se van a cubrir los temas siguientes:

* definición y ejecución de una Máquina de Turing (MT)
* lenguajes decidibles e indecidibles
* tiempo de corrida de una MT 
* substituciones de MT
* MT universal
* problema de la detención
* tesis de Church-Turing

# La palabra "cálculo"

En latín:

* **calx**: piedra caliza.
* **calcŭlus**: piedrita, guijarro para hacer cálculos

¿Que podemos hacer con piedritas?

Se puede hacer operaciones simples:
sumar, restar, comparar, etc.

Y operaciones
más complicadas combinando operaciones básicas: multiplicar, etc.

Cálculo: series de *operaciones* que *se realizan* para obtener un resultado.

# La palabra "compute" (inglés)

* Del francés *computer*
* del Latin *computare* (calcular, contar, sumar),
* de *com* (juntos) + *putare* (limpiar, clarificar, sacar cuentas)
* de *putus* (puro, limpio)

# Computabilidad

¿Cuáles son las cosas que se pueden calcular?

"$f$ es computable si existe un procedimiento efectivo $p$ tal que
  $p(x) = f(x)$ para toda $x$"

Un "procedimiento efectivo" es un cálculo que se puede hacer en el mundo real
(con papel y lapizera).

¿Pero cómo hacemos explicito lo que es un procedimiento efectivo?

# Buscando el modelo

¿Qué modelo podemos proponer para representar matemáticamente
la noción de cálculos del mundo real?

¿Qué problemas podemos resolver?

¿Qué podemos conocer?

# ¿Qué es un problema?

Como problemas, consideramos *funciones booleanas*
sobre un *alfabeto binario*:

$f : \{0,1\}* \mapsto\{0,1\}$

Queremos ser capaces de, dada cualquiera palabra $x \in \{0,1\}*$,
calcular si $f(x)$ vale $1$ o $0$.

Es decir contestar por "si" o por "no" alguna pregunta $f$ acerca de $x$.

Una palabra booleana parece simple pero puede representar muchas cosas:
números, grafos, fórmulas, etc.

De esta forma podemos formular cualquier problema "si/no" acerca de objetos
que se tienen una representación binaria.

# Problemas: ejemplos

"¿$x$ tiene una mayoria de $1$?"

"¿$x$ es de la forma \{0^n, 1^n\}?"

"¿$x$ es representa una formula proposicional satisfacible?"

"¿$x$ representa un grafo que se puede colorar con 3 colores?"

"¿$x$ representa $<G,a,b>$ con $G$ un grafo y existe un camino entre los nodos $a$ y $b$?"

# Definiciones: lenguages y funciones

* Palabra/string binaria: $x \in \{0,1\}*$
* Lenguage: $L \subseteq \{0,1\}*$ (conjunto de palabras)
* Función booleana: $f: \{0,1\}* \mapsto \{0,1\}$
* $f$ es la *función característica* del lenguaje $L_f$ si:
  <br />$L_f = \{ ~ x ~ \mid ~ f(x) =1 , x \in \{0,1\}* \}$
* Calcular $f$ = dado un $x\in\{0,1\}*$, calcular $f(x)$
* Calcular $f$ = Decidir el lenguaje $L_f$
* $f$ es un *problema de decisión*

# Modelos de computación simples

* Circuitos Booleanos

* Automatas Finitos

Ver [Great Ideas in Theoretical Computer Science, Lecture 3](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-080-great-ideas-in-theoretical-computer-science-spring-2008/lecture-notes/lec3.pdf),
de Scott Aaronson (secciones 3 a 6).

# Más poder para los automatas

Hay muchos lenguajes que parecen simples de decidir,
pero los automatas no llegan, por ejemplo:

* capicúas
* $0^n 1^n$

Restricciones de los automatas:

* no pueden volver atrás
* no pueden escribir
* tienen que detenerse cuando llegan al final del input
 
Relajando estas restricciones, vamos a considerar otro modelo de cálculo.

# Máquina de Turing

Propuesta por Alan Turing en el 1936 bajo el nombre "computing machine",
en su artículo "On Computable Numbers".

Características:

* puede leer el input y volver atrás
* puede escribir
* tiene un conjunto finito de estados internos
* puede decidir en qué momento detenerse

#

En casa paso de un cálculo, una máquina de Turing elije:

* cambiar su estado
* escribir en la cinta
* moverse en el input un paso adelande o atrás, o detenerse

Lo hace en función de su estado actual y del símbolo que está leyendo.

#
Una Máquina de Turing es una lista de reglas de
la forma siguiente:

* Estado actual
* Simbolo(s) leido(s)

Implica:

* Estado nuevo
* Simbolo(s) para escribir
* Movimiento(s) cabezal(es)

Generalizamos a máquinas que tienen más de una cinta (1 para el input,
otras para cálculos intermedios).

# Máquinas de Turing: definición formal

Tienen:

* $k$ cintas infinitas hacia la derecha
  (si $k \geq 2$, la primera tiene el input en lectura sola)
* un cabezal por cinta, indica qué celda estamos leyendo,
  se puede mover de un paso en cada direccion
* un conjunto finito de estados internos
* una funcion de transición que describe su comportamiento

Separamos la cinta de input (en lectura sola) de las otras
para poder medir la cantidad de memoria usada por la máquina.

#

<div class="thm">
Una MT es un tuple $(k,\Gamma,Q,\delta)$ con:

* un alfabeto finito $\Gamma \supseteq \{ \square, \triangleright, 0, 1 \}$
  <br />($\square$: "blank", $\triangleright$: "start")
* un conjunto finito de estados
  <br />$Q \supseteq \{q_{start} , q_{acc}, q_{rej}\}$
* una función de transición
  <br />$\delta : Q \times \Gamma^k \mapsto Q \times \Gamma^{k-1} \times \{←,=,→\}^k$
  <br />No está definida en $q_{acc}$ y $q_{rej}$, no escribe $\triangleright$
        ni $\square$, no mueve los cabezales a la izquierda de $\triangleright$.
</div>

# Configuraciones

Llamamos **configuración** de una máquina el estado activo junto
al contenido de sus cintas y la posición de sus cabezales.

La **configuración inicial** con una entrada $x\in\{0,1\}^*$ es:

* $\triangleright x ~ \square ~ \square ~ \square ~ ⋅ ~ ⋅$
* $\triangleright \square ~ \square ~ \square ~ ⋅ ~ ⋅$
* $q_{start}$
* cabezales en la primera celda de cada cinta después del $\triangleright$

# Corrida de una máquina de Turing

Una máquina corre aplicando pasos de acuerdo a su función de transición,
a partir de una configuración inicial.

Dada $x \in \{0,1\}*$, una corrida de una máquina M con entrada $x$ puede:

* alcanzar uno de los estados $q_{acc}$ o $q_{rej}$, en cuales casos
  escribimos $M(x)=1$ si llega a $q_{acc}$ o
  $M(x) = 0$ si llega a $q_{rej}$
* o nunca detenerse y escribimos $M(x)= \infty$

# MT que deciden un lenguaje

<div class="thm">
Una MT M *computa* una función $f : \{0,1\}* \mapsto\{0,1\}$ si para
toda $x \in \{0,1\}*$, $M(x) = f(x)$.

Una MT *decide* el lenguaje $L_f$ si computa su función característica $f$.

Un lenguaje es *decidible* si existe una MT que lo decide.
</div>

# Ejercicio: decidir si una palabra es capicúa.

Enunciar las reglas de una Máquina de Turing que detecta palabras
capicúas.
Es posible definirla con 2 cintas o 1 sola, pero es más simple con
2.

# Ejercicio: máquina sin estado

Aún sin ningun estado interno y solo modificando la cinta
de input, una Máquina de Turing puede decidir lenguajes que un automata no puede.

En el artículo "The uniform halting problem for generalized one state
Turing machines" de Hermann, ver el ejemplo para el lenguaje $A a^n b^m B$ con $n<m$.

Desarrollarlo con el ejemplo `AaabbbB`.

# Otra manera de verlo

Vimos las Máquinas de Turing como automatas con más capacidad.

Otra manera de verlo, es que son
una abstraccion de una persona llevando a cabo un cálculo
usando una hoja de papel.

Alan Turing procedió así:

* describir el comportamiento y los límites de un calculador humano
    * la hoja puede ser una cinta en 1 dimensión sin pérdida de generalidad
    * un cálculo dado requiere una cantidad finita de estados internos para
      representar sus etapas
* introducir la "computing machine" cómo abstracción de un
  calculador humano

# Definiciones alternativas

En algunos lados, se define que una MT tiene una cinta de output, donde se escribe
el resultado del cálculo, y que tiene un solo estado de detencion $q_{halt}$.

En el artículo de Turing ("On Computable Numbers"), las máquinas no
se detienen y producen secuencias infinitas de dígitos de nombres reales.

# Ejercicios

Demostrar:

* Si $L$ es decidible entonces su complemento $\bar{L}$ lo es. 
* Todo lenguaje regular $L$ es decidible.
* El conjunto de lenguajes decidibles es cerrado bajo unión, intersección
y complemento.

# Lenguajes reconocibles
<div class="thm">
Una MT M *reconoce* un lenguaje L si para toda $x \in \{0,1\}^*$,
$x\in L$ ssi $M(x)=1$.
</div>

# Reconocer vs decidir

Si M reconoce L, entonces:

* palabras de L llegan a $q_{acc}$
* las otras o llegan a $q_{rej}$ o hacen que M no termine

Si M decide L, entonces:

* palabras de L llegan a $q_{acc}$
* las otras o llegan a $q_{rej}$

Es decir M siempre se detiene, con cualquier entrada.

# Tiempo de corrida de una MT

<div class="thm">
Una MT calcula $f$ o decide $L_f$ en tiempo $T(n)$ si su corrida en cada entrada $x$
necesita *cómo máximo* $T(|x|)$ pasos.
</div>

**Notación O**: dadas funciones $f,g : \mathbb{N} \mapsto \mathbb{N}$.
Escribimos $f \in O(g)$ si existen una constante
$c > \mathbb{R}^+$ y un rango $n \in \mathbb{N}$ tales que para toda $m > n$, $f(n) \geq c.g(n)$.

Decimos que $f$ es acotada por $g$  (módulo un factor multiplicativo) a partir de un cierto rango
(o en el infinito).

# Observación acerca de los estados

En lugar de pensar el conjunto de estados de una máquina como uno solo,
podemos decir que una máquine tiene más de un estado activo.

Es igual tener $n$ estados activos, cada uno tomando valores en un conjunto $Q_i$,
que un solo estado activo tomando valores en el conjunto $Q_1 \times Q_2 \times \ldots \times Q_n$.

Eso permite "almacenar" información (solo valores dentro de un conjunto fijo).

# Substitución de MT: alfabeto y número de cintas

<div class="thm">
Para toda $f : \{0,1\}^* \mapsto \{0,1\}$
y $T: \mathbb{N} \mapsto \mathbb {N}$ constructible en tiempo,
si $f$ es computable
por una MT $M$ con $k \geq 2$ cintas y un alfabeto $\Gamma$ en tiempo $T(n)$,
entonces es computable por:

1. una MT $M'$ con alfabeto $\{0,1,\square,\triangleright\}$
   en tiempo $(2 log_2(|\Gamma|) + 1)(T(n))$
2. una MT $M''$ con 1 cinta (que sirve de input y working) en tiempo $O(T(n)^2)$
</div>

# Demostración de la substitución 1

Se puede representar cualquier elemento de $\Gamma$ de forma binaria usando
$log_2(\mid\Gamma\mid)$ bits (redondeado para arriba).

Para cada celda de $M$, hay $log_2(\mid\Gamma\mid)$ celdas en $M'$.

Para simular un paso de $M$, $M'$ usa:

1. $log_2(\mid\Gamma\mid)$ pasos para leer los bits que representan un símbolo de $\Gamma$
   Mientra lee, usa sus estados para guardar los símbolos leídos.
2. Según su función de transición (construida en función de la de $M$), calcula los símbolos
   que tiene que escribir y el nuevo estado de $M$
   y guarda esta información en sus estados.
5. En $log_2(\mid\Gamma\mid)$ pasos escribe los bits de los símbolos nuevos en las cintas

Los estados de $M'$ tienen que almacenar: el estado de $M$, $k$ símbolos de $\Gamma$,
un contador de $1$ a $log_2(\mid\Gamma\mid)$.

# Demostración de la substitución 2

En $M''$ las celdas $1, k+1, 2k+1...$ representan la primera
cinta de $M$, las celdas $2, k+2, 2k+2, ...$ la segunda, etc.

Doblamos el tamaño del vocabulario de $M$,
usando para cada símbolo $a$, su versión "marcada" $â$ que representa la posición del cabezal.

Ejecución de $M''$ con entrada $x$:

1. copiar $x$ a la derecha con representación "intercalada" ($O(n^2)$ pasos)
2. simular $M$ paso por paso:
    1. leer la cinta de la izquierda a la derecha, guardando en sus estados
       qué símbolos son leidos por $M$
    2. decidir qué transición de $M$ imitar
    3. volver al principio (después del input) de la cinta, modificándola como corresponde

$M$ corre en $T(n)$ pasos, entonces solo modifica celdas a una distancia de $T(n)$
del principio de cada cinta. El recorrido hecho por $M''$ para cada paso de $M$
dura $2 T(n)$ pasos. Entonces $M''$ corre en tiempo $O(T(n)^2)$.

# Ejercicio

Substitución de MT con cinta infinita en las dos direcciones, por una cinta infinita en
una sola dirección.

# Más resultados similares

Más resultados de cambio de tipo de MT con desaceleración polinomial:

* pasar de cintas con $k$ dimensiones a cintas con $1$ dimensión
* pasar de memoria en acceso random a memoria en acceso secuencial
* no permitir que los cabezales no se muevan 

# Resolver problemas con dominios otros que palabras binarias

Ver [slides 23 a 30](http://stellar.mit.edu/S/course/6/sp13/6.045/courseMaterial/topics/topic8/lectureNotes/6045-Lecture_7/6045-Lecture_7.pdf).

Por el momento, no es necesario entrar en los detalles de codificacion/representacion.

Idea: como entrada de una TM se puede representar

* grafos: lista de pares o matriz de adyacencia
* automatas: conjunto de estados, tabla de transiciones
* otras TM

Dado un entero/grafo/automata/MT $x$, escribimos $<x>$ su representación como palabra binaria.

# Máquinas de Turing como palabras

¿Qué información es requerida para describir una TM?

* su cantidad de cintas $k$
* su conjunto de símbolos $\Gamma$
* su conjunto de estados $Q$
* su fonción de transición $\delta$

Por ejemplo dada una TM $M=<2,\Gamma, Q, \delta>$, podemos armar
una palabra $<k,\Gamma,Q,\delta>$ con $<\delta>$ siendo el grafo de $\delta$
representado como una lista:
 <br />$<q_0,s_0,s_0,q_n,s_w,z_i,z_w>, <...>, <...>, ...$
 (con $z_i, z_w \in \{←,=,→\}$).

#
Consecuencia: podemos establecer una relación uno-a-uno $m$ de
los enteros naturales a las máquinas de Turing.

Consideremos la representación binaria del entero natural $n$.
Si representa una Máquina de Turing $M_n$ decimos que $m(n) = M_n$,
sino $m(n) = X$ , con $X$ una maquina de Turing
arbitraria que entra en un bucle infinito con todo input.

# Mala noticia

cardinalidad de todas las máquinas de Turing
< cardinalidad de todos los lenguajes

¿Porqué?

1. Cardinalidad de las MT = Cardinalidad de los enteros
2. Cardinalidad de todos los lenguajes = Cardinalidad de los reales

2. se demuestra con un argumento de diagonalisación de Cantor.

Las máquinas de Turing no alcanzan para decidir todos los lenguajes.

# Maquina de Turing Universal

Representando MT como palabras, podemos disenar MT que toman descripciones
de otras MT como entrada y hacen cosas divertidas.

En su articulo de 1936, justo después de presentar sus Máquinas, Alan Turing
describe una máquina **universal**.

Una máquina U es *universal* si para cualquiera máquina M y palabra $x\in \{0,1\}*$,
$U(<M,x>) = M(x)$.

<div class="thm">
Existe una máquina de Turing universal.
</div>

# Consecuencia de la existencia de una MTU

Si una máquina es universal, puede leer la descripcion de otra máquina y
"ejecutarla" con una palabra de entrada.

Sin máquina universal, tendríamos que construir (físicamente) una máquina por tarea.

Con máquinas universales, tenemos software (máquinas simuladas) corriendo
arriba de hardware (máquinas universales).

# Ejercicio: construcción de una MTU

<div class="thm>
Existe una máquina U *universal* con dos cintas
</div>

Construir una MTU U, capaz de simular cualquier MT $M$ con $k$ cintas.

Requiere: lemma k cintas trabajo a 1 cinta trabajo.

¿Cúales son las fases principales de la ejecución de MTU?

¿Cómo simular un paso de M?

¿En cúanto tiempo corre $U(<M,x>)$ con respeto a $M(x)$?

<!--
# Unos lenguajes sobre $\{0,1\}^*$

* PAL = $\{ x \mid x$ es capicúa $\}$
* $\lfloor x \rfloor$ = representación de $x$ como string binaria
* EVEN = $\{ \lfloor x \rfloor \mid x$ es par $\}$
* SMALLER = $\{ \lfloor (x,y) \rfloor  \mid x \leq y \}$
* PRIMES = $\{ \lfloor x \rfloor  \mid x$ es primo $\}$
* HAM = $\{ \lfloor G \rfloor  \mid G$ es hamiltoniano $\}$
* SUBSETSUM = $\{ (X,x) \mid \exists S\subseteq X, \Sigma S = x \}$
  (con X conjunto de enteros, x entero)
* INDSET =
  <br />$\{(G,k)\mid\exists S\subseteq V(G),|S|\geq k,\forall uv\in S,uv\notin E(G)\}$
  (con $G$ un grafo, $V(G)$ los puntos de $G$, $E(G)$ los ejes de G)
-->

# Problemas indecidibles

Hemos observado que por una diferencia de cardinalidades, hay lenguajes
(de hecho, la gran mayoría) que no son decidibles.

Ahora que sabemos representar MT como palabras
vamos a ver lenguajes indecidibles particulares que involucran MT.

Las demostraciones de indecidibilidad van a involucrar MTU.

# Indecibilidad de la aceptación por TM

* $  Acc_{MT} = \{<M,x> \mid$  $M$ es una MT y acepta $x \}$
* $ Halt_{MT} = \{ <M,x> \mid$ $M$ es una MT y se detiene con entrada $x \}$
* $Empty_{MT} = \{ <M,x> \mid$ $M$ es una MT y $L(M)=\emptyset \}$

# Indecibilidad de $Acc_{MT}$

$Acc_{TM} =\{<M,x> \mid$  $M$ es una MT y acepta $x \}$

> * Suponer que $Acc_{MT}$ es decidible
> * Es decir existe MT $H$ que decide $Acc_{MT}$
> * ¿En qué casos tiene que aceptar? ¿Rechazar?
> * Diseñar $H'$ a partir de $H$, que decide si una TM $M$ acepta su propia representacion $<M>$
> * Diseñar $D$ (diagonal machine) que hace lo contrario de $H'$
> * Considerar que pasa con $D(<D>)$.
> * Contradicción.

# ¿Qué pasó?

Dibujar una tabla con las MT etiquetando cada línea, y
las palabras representando las MT etiquetando cada columna.

La diagonal central representa el resultado de $M(<M>)$ para toda $M$.

$D$ está construida para ser distinta de esa diagonal, entonces $D$ no
puede aparecer como etiqueta de una linea en esa tabla.

Contradiccion dado que supusimos que la tabla incluía a todas las MT.

# Indecibilidad de $Halt_{MT}$

$Halt_{MT} = \{ <M,x> \mid$ $M$ es una MT y se detiene con entrada $x \}$

$Halt_{MT}$ es el **problema de la detención** (Halting Problem).

Decidir $Halt_{MT}$ significa poder detectar si una MT va a entrar
en un bucle infinito sin ejecutarla.

Por ejemplo podríamos demostrar o infirmar la
Conyectura de Goldbach (todos los números $\geq 4$
son la suma de dos números primos):

    for i = 2 to infinity:
        if 2*i is not the sum of two primes:
            then HALT

Si $Halt_{MT}$ fuera decidible podríamos detectar si este programa terminara.

Suena demasiado lindo...

# Ejercicio: Indecibilidad de $Halt_{MT}$

$ Halt_{MT} = \{ <M,x> \mid$ $M$ es una MT y se detiene con entrada $x \}$

La idea es mostrar que su pudieramos decidir $Halt_{MT}$, podriamos decidir $Acc_{TM}$.

# Adecuación de las MT

¿Las MT representan todos los procedimientos efectivos?

Como un procedimiento efectivo es una noción informal, no lo podemos demostrar.

Pero algunas cosas indican que sí, es el caso.

Se demostro que tres modelos de calculo:

* funciones recursivas de Kurt Gödel y Jacques Herbrand
* el calculo lambda de Alonzo Church
* las máquinas de Alan Turing

Eran equivalentes, ie calculaban las mismas funciones.

La hypothesis que las máquinas de Turing representan todos los procedimientos
efectivos se llama **Tesis de Church-Turing**.

# Resultados importantes

* las máquinas de Turing formalisan la idea intuitiva de lo que es
  un cálculo
* tienen una descripción finita y son enumerables
* la gran mayoría de los lenguajes y funciones son indecidibles
* ciertos problemas interesantes, como el problema de la detención,
  son indecidibles
* varios tipos de máquinas tienen una capacidad de computo
  equivalente
* existe una máquina universal, que puede leer la descripción de
  cualquier otra máquina, e imitar su comportamiento

# Lectura

1. Arora y Barak: Capítulo 1 hasta 1.5 incluido
   (Sin sección 1.2.1, remark 1.7, claim 1.8, section 1.5.2).

2. Sipser: Capítulo 0.
   Capítulo 4, sección 2: lenguajes indecidibles y problema de la detención.

# Más lectura

* Pippenger y Fischer, "Relations among complexity measures", 1979: simulación de MT con "cintas" k-dimensionales
  por MT con cintas unidimensionales
* Dick Lipton
  [Programming Turing Machines is Hard](http://rjlipton.wordpress.com/2010/01/27/programming-turing-machines-is-hard/):
  substitución de MT multicinta en tiempo $O(T(n) log(T(n)))$ (en lugar de $O(T(n)^2)$)
* [Church-Turing thesis en Wikipedia](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis)


