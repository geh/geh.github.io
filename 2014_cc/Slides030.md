% Complejidad Computacional
  <br />Unidad 3: Complejidad espacial

# Resumen Unidad 2

* consideramos clases de lenguajes
* son definidas por la cantidad de tiempo o de espacio necesario para decidir un lenguaje
* **P**: clases de los lenguajes fáciles
* más recursos $\rightarrow$ inclusiones estrictas de clases de complejidad
* reducciones entre lenguajes
* lenguaje difícil (hard) o completo (complete) con respeto a una clase
* clase **NP** definida con certificados o con nondeterminismo
* 3SAT (y varios otros lenguajes) es **NP** completo

# Preguntas

* ¿implicaciones de **P**=**NP**?
* ¿implicaciones de **P**$\neq$ **NP**?

# Contenido Unidad 3

* cálculos acotados por espacio
* clases PSPACE, NPSPACE
* relación con P, NP, EXPTIME
* problemas completos para clases espaciales
* clases espaciales sublineales (L, NL)

Referencias: Sipser cap. 8, Arora Barak cap. 4

# **SPACE**

<div class="thm">
Sea $s:\mathbb{N}\mapsto{N}$ y $L\subseteq \{0,1\}^*$.

Decimos que
$L\in$**SPACE**$(s(n))$ si existe una MT M y una constante $c$
tal que M decide $L$ y además, para todo input de longitud $n$,
$M$ visita cómo máximo $c s(n)$ casillas en sus cintas
(excluyendo la cinta de input)
durante la computación.

</div>

Observación: como $M$ decide $L$, $M$ se detiene después de un número
finito de pasos.

# **NSPACE**

<div class="thm">
Sea $s:\mathbb{N}\mapsto{N}$ y $L\subseteq \{0,1\}^*$.

Decimos que $L\in$**NSPACE**$(s(n))$ si existe una MTND M cuyas ejecuciones
nondeterministas cumplen con el mismo límite de espacio.

</div>

Observación: los caminos que no llegan a $q_{accept}$ pueden ser infinitos.

# Precaución

Trabajamos con cotas espaciales $s:\mathbb{N}\mapsto\mathbb{N}$
que son *constructibles en espacio*, ie:

* $s(n) \geq log(n)$
* existe una MT que computa $s(|x|)$ en espacio $O(s(|x|))$ dada
  $x$ como input.

# Simulación universal

Asumimos lo siguiente:

Se puede hacer una simulación universal de una máquina
con $k$ cintas de trabajo por una máquina con una sola cinta de trabajo,
con sólo un gasto de espacio más grande por una constante multiplicativa.

Entonces, podemos asumir sin perdida de generalidad que hablamos de
complejidad espacial con máquinas con solo 1 cinta de trabajo.

# Space hierarchy theorem

<div class="thm">
Si $f ,g$ son funciones constructibles en espacio tales que
$f(n) \in o(g(n))$, entonces

**SPACE**$(f(n))$ $\subset \neq$ **SPACE**$(g(n))$

</div>

Demo: a lo time hierarchy theorem.

Stearns, Hartmanis y Lewis, *Hierarchies of memory limited
computations* 1965

Sipser Sección 9.1: Space hierarchy theorem

# **TIME** y **SPACE**

En cada paso, una MT determinista puede descubrir cómo máximo
un número constante de casillas nuevas, entonces:

<div class="thm">
**TIME**$(s(n)) \subseteq$ **SPACE**$(s(n))$
</div>

# Configuraciónes

Una *configuración* de una MT M es el contenido de todas las casillas
no blancas de las cintas, las posiciones de sus cabezales y su estado.

Si una MT corre en espacio $O(s(n))$, entonces
una configuración ocupa un espacio $O(s(n))$.

# Grafo de configuraciones

Sea M una MT determinista o nondeterminista, $x\in\{0,1\}^*$.

El *grafo de configuraciones* de M, llamado $G_{M,x}$, es un grafo
dirigido cuyos nodos corresponden a todas las configuraciones
posibles de M cuando el input es $x$.

$G_{M,x}$ tiene un eje de la configuración $C$ a la configuración
$C'$ si $C'$ es alcanzable a partir de $C$ en un paso según la(s)
función(es) de transición de M.

Podemos siempre modificar M para que borre su cinta y ponga sus
cabezales en posición inicial cuando acepta el input
$\rightarrow$ hay sólo una configuración final $C_{accept}$.

---

<div class="thm">
Sea M una MT(ND) usando espacio $s(n)$. El número de configuraciones
$C_M(n)$ de $M$ con input $n$ es acotado por:

$C_M(n) \leq |Q_M| \cdot n \cdot s(n) \cdot |\Sigma_M|^{s(n)}$

con $|Q_M|$ los estados de $M$, $|\Sigma_M|$ su alfabeto.

En particular si $s(n) \geq log(n)$ tenemos $C_M(n) = 2^{O(s(n))}$.

</div>

---

<div class="thm">
Para toda $s:\mathbb{N}\mapsto\mathbb{N}$ constructible en espacio:

**TIME**$(s(n))$ $\subseteq$ **SPACE**(s(n)) $\subseteq$ **NSPACE**$(s(n))$
   $\subseteq$ **TIME**$(2^{O(s(n))})$
</div>

---

Demo  **SPACE**(s(n)) $\subseteq$ **TIME**$(2^{O(s(n))})$:

Sea $L \in$ **SPACE**$(s(n))$ y $M$ una MT corriendo en espacio $O(s(n))$
decidiendo $L$. Consideramos el cálculo de $M(x)$ con $x$ input de longitud $n$.

Hay como máximo $C_M(n) = 2^{O(s(n))}$ configuraciones de $M$ con input $x$,
pero si $M(x)$ repite una configuración entonces buclearía y nunca se detendría.

Entonces $M$ con input $x$ se detiene en $C_M(n) = 2^{O(s(n))}$
pasos.

---

Demo **NSPACE**$(s(n))$ $\subseteq$ **TIME**$(2^{O(s(n))})$ por
"reachability method".
Sea $M$ que decide $L \in$**NPSPACE**$(s(n))$:

* Generar el grafo de configuracion $G_{M,x}$ en tiempo $2^{0(s(n))}$
* Chequear en tiempo lineal en función del tamaño de $G_{M,x}$ si
  $C_{accept}$ es alcanzable a partir de $C_{start}$ (por ej. depth-first
  search)

# Definiciones clases espaciales

<div class="thm">
**PSPACE** = $\bigcup_{c>0}$ **SPACE**$(n^c)$

**NPSPACE** = $\bigcup_{c>0}$ **NSPACE**$(n^c)$

**L** = **SPACE**$(log(n))$

**NL** = **NSPACE**$(log(n))$

</div>

# Ejemplo

3SAT $\in$ **PSPACE**

Dado input $\lfloor \varphi \rfloor$, buclear enumerando todas las asignaciones
de variables de $\varphi$ y chequear el valor de $\varphi(z_i)$.

Espacio usado para las asignacíones: polinomial (reutilizar para cada
asignación nueva).

Espacio usado para el chequeo: polinomial (cuota espacial de un algoritmo
determinístico en tiempo polinomial).

# Generalización: **NP** $\subseteq$ **PSPACE**

Sea $L\in$**NP**.

Dado input $x$, buclear sobre todos los certificados posibles
de $x$ con respeto a $L$. Se puede reutilizar el espacio (polinomial)
usado para alojar un certificado, para el certificado siguiente.

El chequeo de cada certificado se hace en tiempo determinístico polinomial
en función de $|x|$, entonces en espacio polinomial en función de $|x|$.

# PATH

PATH = $\{ \lfloor G,s,t \rfloor \mid G$ es un grafo dirigido
 con camino de $s$ a $t \}$

<div class="thm">
PATH $\in$ **NL**
</div>

Sea $n$ la cantidad de nodos en el grafo.

La idea del algoritmo es explorar todos los caminos de longitud $< n$ posibles
a partir de $s$, pasar al estado $q_{accept}$ si
encontramos $t$.

Solo necesitamos guardar dos cosas en memoria:

* un puntero para referirse al nodo actual: tamaño $log(n)$
* un contador para guardar la longitud del camino
  visitado: tamaño $log(n)$

# **PSPACE** completitud

<div class="thm">
Un lenguaje $L$ es **PSPACE** hard si para todo
$L' \in$ **PSPACE**, $L' \leq_p L$.

Si además $L \in$ **PSPACE**, $L$ es **PSPACE** completo.

</div>

Usamos las mismas reducciones que para **NP**, ie,
$f$ corriendo en tiempo polinomial.

# Un lenguaje **PSPACE** completo

Ya vimos ese truco con **NP**.

SPACESAT =
 $\{ \lfloor M,w,1^n \rfloor \mid$ M MTD, acepta $w$
  en espacio $n\}$

<div class="thm">
SPACESAT es **PSPACE** completo.
</div>

>* SPACESAT $\in$ **PSPACE**: se puede decidir si $x\in$SPACESAT con una máquina universal
   de Turing (que usa tanto espacio como M modulo una constante multiplicativa)
   que verifica que M acepta $w$ sin usar más de $n$ espacio
>* Sea L $\in$ **PSPACE**: existe $M$ que decide L usando espacio $O(n^d)$.
   Para un $x\in\{0,1\}^*$ construimos $f(x) = \lfloor M, x, 1^{n^d}\rfloor$.
   Entonces L $\leq_p$ SPACESAT.

# Quantified Boolean Formulas

Una Quantified Boolean Formula es una formula
proposicional con cuantificación ($\forall$ y $\exists$)
sobre sur variables booleanas.

Una QBF está en forma *prenexa* si tiene todos los
cuantificadores primeros:

$Q_1 x_1 Q_2 x_2 \ldots Q_n x_n ~ \varphi(x_1,x_2,\ldots,x_n)$

Asumimos que todas las variables están cuantificadas.

---

Una QBF es verdadera o falsa (todos los $x_i$ son cuantificados).

Verdadera:
$\forall x \exists y . (x \wedge y) \vee (\neg x \wedge \neg y)$

Falsa:
$\forall x \forall y . (x \wedge y) \vee (\neg x \wedge \neg y)$


---

Se puede reescribir una QBF $\varphi$ en una formula
proposicional $\varphi'$ equisatisfacible, pero de tamaño
exponencialmente más grande:

* $\forall x_i \varphi$ se reescribe en
  $\varphi[x_i \leftarrow 0] \wedge \varphi[x_i \leftarrow 1]$
* $\exists x_i \varphi$ se reescribe en
  $\varphi[x_i \leftarrow 0] \vee \varphi[x_i \leftarrow 1]$

Una formula booleana es una QBF sin cuantificadores
existenciales.




# TQBF

TQBF = $\{ \lfloor \varphi \rfloor \mid$ formulas booleanas cuantificadas verdaderas $\}$

<div class="thm">
TQBF es **PSPACE** completo

</div>

Stockmeyer y Meyer, *Word problems requiring exponential time*, 1973


# TQBF $\in$ **PSPACE**

Describimos el algoritmo $A$ para TQBF:

Sea $n$ el número de variables del input $\psi$,
y $m$ su tamaño.

Escribimos $\psi[x_i=b]$ para
$\psi$ con $x_i$ reemplazado por $b\in\{0,1\}$.

Si $n=0$ se puede evaluar $\psi$ en tiempo
(y espacio) $O(m)$.

Si $n>0$:

* si $\psi=\forall x_i \varphi$: return
  $A(\varphi[x_i = 0])$ && $A(\varphi[x_i = 1])$
* si $\psi=\exists x_i \varphi$: return
  $A(\varphi[x_i = 0])$ || $A(\varphi[x_i = 1])$

---

Sea $s_{n,m}$ el espacio usado para una llamada
a $A$.

Una vez terminada una llamada recursiva a
$A$, se puede reutilizar el espacio usado.

$A$ usa $O(m)$ espacio para escribir $\psi[x_i=b]$.

$s_{n,m} = s_{n-1,m} + O(m) = O(n \cdot m)$

$A$ necesita un espacio polinomial y decide TQBF.

# TQBF es **PSPACE** hard

> * Mostramos que $L \leq_p TQBF$ para cualquier
>   $L\in$ **PSPACE**.
> * Sea $M$ una MT decidiendo un lenguaje $L$ y corriendo
>   en espacio $O(s(n))$, y sea $x\in\{0,1\}^n$.
>   La idea es construir una QBF de tamaño $O(s^2(n))$ que es
>   verdadera ssi $M$ acepta $x$.
> * Una configuración de $M(x)$ es de tamaño $m=O(s(n))$.
> * Lemma: se puede construir una formula booleana $\varphi_{M,x}$
>   de tamaño $O(m)$
>   tal que para $C,C' \in \{0,1\}^m$, $\varphi_{M,x}(C,C')=1$ ssi
>   $C$ y $C'$ codifican configuraciones sucesivas en el grafo
>   de configuraciones de $M(x)$
> * Construimos $\psi$ una QBF de tamaño polinomial tal que $\psi$
>   es verdadera ssi existe un camino desde $C_{start}$ hasta
>   $C_{accept}$

---

> * Construimos una secuencia de QBF
>   $\psi_i(C,C')$ verdadera ssi existe camino de longitud
>   $2^i$ de $C$ a $C'$
> * Obs 1: $\psi_0=\varphi_{M,x}$ , $\psi = \psi_m$
> * Obs 2: hay un camino de tamaño máximo $2^i$ de $C$ a $C'$
>   ssi hay un camino de tamaño máximo $2^{i-1}$ de $C$ a $C''$
>   y un camino de tamaño máximo $2^{i-1}$ de $C''$ a $C'$.
> * Tenemos ganas de definir:
>   $$\psi_i(C,C') = \exists C'' ~ \psi(C,C'') \wedge \psi(C'',C')$$
> * Sería correcto pero $\psi_m$ de tamaño $O(2^m)$.
> * $\psi_i(C,C') =$
>   $\exists C'' ~ \forall D,E ~ 
>   ( (D=C \wedge E=C'') \vee (D=C'' \wedge E=C') )$
>   $\rightarrow \psi_{i-1}(D,E)$
> * $|\psi_i| \leq |\psi_{i-1}| + O(m)$
> * $|\psi_m| \leq O(m^2)$
> * Se puede generar $\psi_m$ de tal forma que esté ya en forma prenexa

# Observación

* la demostración anda para $L\in$**NPSPACE**
* entonces **PSPACE**=**NPSPACE**


<!--

# Ejercicio con QBF

Juego con 2 jugadores: Jugador X y Jugador O.

Juegan con una cinta de $n$ casillas inicialmente vacías.

Cada vez que le toca a alguien, tiene que poner su símbolo o en la primera,
o las dos primeras casillas desocupadas de la izquierda.

Gana él que pone su símbolo en la última casilla.

Jugador X empieza.

Escribir una QBF $\varphi(n)$ que es valida si $x$ tiene una estrategia ganadora
con una cinta de longitud $n \geq 2$.
-->

# Teorema de Savitch (1970)

<div class="thm">
Para $s:\mathbb{N}\mapsto\mathbb{N}$ constructible en espacio:

**NSPACE**$(s(n))$ $\subseteq$ **SPACE**$(s(n)^2)$

</div>

# Demo Savitch

* Sea L $\in$ **NPSPACE**$(S(n))$
* decidido por máquina cuyo grafo
  de config de tamaño $\leq M = 2^{O(s(n))}$
* decidir $x \in L$ es equivalente a decidir si $C_{accept}$ alcanzable 
  desde $C_{start}$
* definir procedimiento REACH$(u,v,i)$ que chequea si hay camino
  |u $\rightarrow$ v| $\leq 2^i$
* enumerar nodos del grafo (usa espacio $O(log(M))$,
  devolver "sí" si hay un $z$ tal que REACH$(u,z,i-1)$ y  REACH$(z,v,i-1)$.
* profundidad recursiva: $n$
* sea $s_{M,i}$ el espacio usado por REACH$(u,v,i)$
* entonces $s_{M,i} = s_{M,i-1} + O(log M)$
* entonces $s_{M,i} = O(log^2 M) = O(s(n)^2)$

# **NL**, **L**

<div class="thm">
**L** = **SPACE**$(log(n))$

**NL** = **NSPACE**$(log(n))$

</div>


¿Que se puede alojar en espacio log?

* Número constante de contadores hasta la longitud del input
* Número constante de puntadores hacia la cinta de input

# Ejemplos

Ya vimos que $PATH\in$ **NL**.

$uPATH\in$**L** (Reingold 2004)

$\{0^k1^k \mid k \geq 0 \} \in$ **L**:

* contar el número de 0 y el número de 1 que están
  en la cinta de input. Cada contador ocupa espacio
  $O(log(n))$
* comparar los dos contadores

# Transductor logspace

<div class="thm">

Un *transductor logspace* es una máquina con 3 cintas:

1. de input en lectura sola
2. de trabajo en lectura/escritura donde el consumo de espacio es $O(log(n))$
3. de output en *escritura sola*:
  su cabezal puede o quedarse inactivo
  o escribir un símbolo y avanzar

Un transductor $M$ computa una función (en espacio logarítmico)
$f:\{0, 1\}^* \mapsto \{0,1\}^*$ si para todo input $x$,
$M$ se detiene con $f(x)$  escrito
en la cinta de output.

</div>

¿Tamaño posible de $f(x)$?

# Reducciones logarítmicas

<div class="thm">
Si para dos lenguajes $B,C$, existe una función $f$ computable
en espacio logarítmico tal que para todo $x\in\{0,1\}^*$,
$x\in B$ ssi $f(x)\in C$,
entonces decimos que $B$ es *reducible en espacio logarímico* a $C$
(escrito $B \leq_L C$).

$C\in$**NL** es **NL** completo si para todo
$B \in$ **NL**, $B \leq_L C$.

</div>

# Composición en espacio log

<div class="thm">
Si $B \leq_L C$ y $C \in$ **L** entonces $B\in$ **L**.

</div>

> * Sea $T$ el transductor logspace de la reducción, y $M_C$
la máquina que decide $C$ en espacio log.
> * Mostramos que existe $M_B$ que decide $B$ en espacio log.
> * Primer intento: armar $M_B$ como una máquina que ejecuta
>   $T$ y luego $M_C$.
> * Pero eso necesita generar $f(x)$ completamente antes
>   de ejecutar $M_C$.
>   $f(x)$ puede ocupar más espacio que logarítmico.

---

Lemma:
<div class="thm">
Si $T$ es un transductor logspace que computa $x \mapsto f(x)$,
entonces existe $T'$ que computa
$\lfloor x,i \rfloor \mapsto f(x)_i$ en espacio log.

</div>

> * Idea:<br />
>   $T'$ tiene un contador inicializado a $i$.<br />
>   Cuando $T$ va para escribir un bit de $f(x)$,
>   $T'$ no escribe nada pero decementa el contador.<br />
>   Cuando el contador llega a $0$, $T'$ escribe el bit $f(x)_i$ y se detiene.
> * Ese contador ocupa un espacio
>   $O(log \left|f(x)\right|) = O(log \left|x\right|)$

---

Volviendo a la demostración de $B \in$ **L**: 

> * $M_B$ controla donde está el cabezal de lectura de $M_C$
>   en $f(x)$, y ejecuta el cálculo de $f(x)_i$ por $T$
> * Puede ser que compute varias veces el mismo bit de $f(x)$,
>   entonces es ineficiente en tiempo, pero es eficiente en espacio.
> * $M_B$ corre en espacio
>   $O( log \left| f(x) \right|)$ + $O(s(\left|x\right|))$ + $O(s'(\left|f(x)\right|))$,
>   con $s$ y $s'$ el espacio de $T$ y $M_C$
> * entonces $M_B$ corre en espacio $O(log\left|x\right|)$

# PATH es **NL** completo

> * sea $B\in$**NL**, mostramos $B\leq_L PATH$
> * $M$ es nondeterminista y decide $B$ en espacio $O(log(n))$
> * usamos $x \mapsto f(x)=\lfloor G_{M,x}, C_{start}, C_{accept} \rfloor$
> * $M(x)=1$ ssi existe camino de $C_{start}$ a $C_{accept}$
> * $x \mapsto f(x)$ es calculable en espacio log:
>     * representar $G_{M,x}$ como matriz de adjacencia
>     * para escribir un bit de esa matriz, computar si
>       las configuraciones $C$ y $C'$ se siguen según las
>       funciones de transición de $M$
>     * se hace en espacio $O(|C| + |C'|) = O(log |x|)$
>       deterministicamente

# Observaciones

> * un transductor logspace tiene un tiempo de corrida polinomial
> * entonces si $B$ es reducible en espacio log a $C$,
>   también $B$ es reducible en tiempo polinomial a $C$
> * se puede definir **NP** completitud con
>   reducciones logspace (el libro de Papadimitriou lo hace),
>   y ¡nada se rompe!
> * es decir, no conocemos ningun ejemplo de problema en **NP** que sea
>   completo para reducciones en tiempo polinomial, y no para
>   reducciones en espacio log
> * Se puede modificar la prueba del teorema de Cook-Levin
>   que vimos de manera que sea en espacio log

# Certificados para **NL** 

> * podríamos pensar que **NL** es la clase de problemas que
>   tienen soluciones chequeables en espacio log
> * problema: SAT tiene soluciones chequeables en espacio log (no obvio pero lo asumimos)
> * tendríamos **NP** $\subseteq$ **NL**, poco probable dado que
>   **NL** $\subseteq$ **P**
> * ¿qué es lo que hay que arreglar en esa definición?

# Definición alternativa de **NL**

<div class="thm">
$B\in$**NL** si existe una MT $M$ determinista con
una cinta adicional en *lectura unidireccional*, y un polinomio
$p:\mathbb{N}\mapsto\mathbb{N}$ tal que para todo $x\in\{0,1\}^*$:

$x\in B \leftrightarrow
   \exists u \in \{0,1\}^{p(\left| x \right|)} s.t. M(x,u)=1$

Con:

* $M(x,u)$ el output de $M$ con $x$ puesto en su cinta de input
  y $u$ puesto en su cinta en lectura unidireccional
* $M$ corre en espacio $O(log(\left|x\right|))$

</div>

# Equivalencia de definiciones

Mostramos que $B \in NL_{nd} \Leftrightarrow B \in NL_{cert}$

$\exists$ MTND $N$ logspace $\Leftrightarrow$ $\exists$ MTD M logspace con certificado

> * $N$ corre en tiempo polinomial
> * las elecciónes de $N$ forman el certificado $u$ de $M$
> * La definición de $\delta$ de $M$ sigue las $\delta_0$ y $\delta_1$
>   de $N$, en particular:<br />
>   $\delta(0,...) = \delta_0(...)$<br />
>   $\delta(1,...) = \delta_1(...)$<br />
>   $0$ o $1$ siendo el bit leido por $M$ en la cinta del certificado
> * dado que $N$ usa espacio log, $M$ también

# Teorema de Immerman-Szelepcsényi

<div class="thm">
$coPATH \in$ **NL**

</div>

# Demostración

Mostramos que hay un algoritmo A corriendo en espacio $O(log (n))$ tal que:

$A(G,s,t,u)=1$ ssi $t$ no es alcanzable desde $s$ en $G$, con certificado $u$
en lectura unidireccional.

Llamamos:

* $n$ el número de nodos de $G$
* $C_i$ el conjunto de nodos alcanzables desde $s$ en $\leq i$ pasos
* $c_i$ el tamaño de $C_i$

---

Para cualquier input, A ya sabe:

$C_0 = \{s \}$

$c_0=1$

$v \in C_i$ puede ser chequeado facilmente:
un certificado $path_i(s,v)$ en lectura unidireccional es
la secuencia de nodos $v_0,v_1, \ldots, v_k$ del camino de $s$ a $v$ ($k\leq i$).

---

Además de $path_i(s,v)$ necesitamos dos tipos de certificados:

1. $noPath_i(s,v)$:<br />
   certificado para $v \notin C_i$, asumiendo que el verificador
   ya conoce el valor $c_i$.
2. $size_i(k)$:<br />
   certificado para $c_i = k$, asumiendo que el verificador ya conoce
   el valor $c_{i-1}$.

Con certificados de tipo 2 nos podemos enterar de los valores
$c_1, \ldots, c_n$, y al final con un certificado de tipo 1,
convencernos que $t \notin C_n$.

---

Certificado $noPath_i(s,v)$, asumiendo $c_{i-1}$ está conocido:

> $v_1, path_{i-1}(s,v_1), \ldots, v_{c_{i-1}} , path_{i-1}(s,v_{c_{i-1}})$

con $v_1, \ldots v_{c_{i-1}} \in C_{i-1}$.

Se puede chequear que

1. el número de nodos del certif. es exactamente $c_{i-1}$
2. los nodos están listados en órden estrictamente creciente
   (para no engañar a la verificadora)
3. ningun de los nodos listados es $v$ ni un vecino de $v$
4. cada certificado $path_{i-1}(s,v_j)$ es correcto

en espacio $O(log(n))$ con certificado en lectura unidireccional.

---

Certificado $size_i(k)$, asumiendo $c_{i-1}$ está conocido:

> $v_1, (no)Path_i(s,v_1), v_2,(no)Path_i(s,v_2),\ldots, v_n, (no)Path_i(s,v_n)$

dependiendo de si $v\in C_i$ o no.

Se puede chequear que

1. los nodos están listados en órden estrictamente creciente
2. cada certificado $path_i(s,v)$ o $noPath_i(s,v)$ es correcto
3. el número de nodos en $C_i$ es exactamente $k$

en espacio log. con certificado en lectura unidireccional.

---

Un certificado de $(G,s,t) \notin PATH$ es:

$size_1(c_1), size_2(c_2), \ldots , size_{n-1}(c_{n-1}), noPath_n(s,t)$

Cada certificado $size_i(c_i)$ puede ser chequeado en espacio log. y después de
cada chequeo el verificador sólo necesita alojar $c_i$.

Entonces todo el chequeo se hace en espacio log.

# Corolario

<div class="thm">
Si $s:\mathbb{N}\mapsto\mathbb{N}$ constructible en tiempo
($\geq log(n)$), entonces
**NSPACE**$(s(n))$ = **coNSPACE**$(s(n))$.

</div>

---

Demo: Sea $B\in$**coNSPACE**$(s(n))$.

Entonces existe MTND $M$ que usa espacio $s(n)$ tal que
$x\in B$ ssi ninguna secuencia de elecciones de $M$ con input $x$ llega a $q_{accept}$.

Existe un transductor $T_B$ que computa $x \mapsto \lfloor G_{M,x}, C_{start}, C_{accept} \rfloor$
en espacio $O(s(n))$.

$noPATH \in$ **NL** ie existe MTND $N$ que decide $noPATH$ en espacio log. 

Componiendo $T_B$ y $N$ de manera perezosa, obtenemos $M'$ que decide $B$ en espacio $O(s(n))$,
ie, $B\in$**NSPACE**$(s(n))$.

# Referencias

* Arora y Barak: Capítulo 4
* Sipser: Capitulo 9
* Stearns, Hartmanis y Lewis. *Hierarchies of memory limited
  computations*. In FOCS, pages 179–190. IEEE, 1965.
* Stockmeyer y Meyer. *Word problems requiring exponential time*.
  In STOC, pages 1–9. ACM, 1973.
* Savitch. *Relationships between nondeterministic and deterministic tape
  complexities*. J. Comput. Syst. Sci., 4:177–192, 1970.
* [Matthew Johnson, Lectures Notes 2009](http://www.dur.ac.uk/matthew.johnson2/teaching/acc/lectures/lecture12handout.pdf)
* [Nabil Mustafa, Lecture Notes, 2008](http://russell.lums.edu.pk/~cs514s07/slides/lecture12.pdf)
* Immerman. *Nondeterministic space is closed under complementation*. SIAM
  J. Comput., 17(5):935–938, 1988.
* Szelepcsényi. *The method of forcing for nondeterministic automata*. Bulletin
  of the European Association for Theoretical Computer Science, 33:96–100, Oct. 1987.
  Technical Contributions.



