% Complejidad Computacional
  Semana 3: **P** versus **NP**, Problemas **NP** intermedios

# Teorema de Cook-Levin

Dados L $\in$ **NP** y una string $x$,
construimos $\varphi_x$ tal que $x\in L$ ssi
$\varphi_x \in$ CNF-SAT.

Idea: Sea M una verificadora de L indiferente y con 2 cintas.

$\varphi_x$ es un "patrón de trazas" de las corridas
exitosas de M con input $x$ y $u$ ($u$ certificado de $x$).

---

* supongamos $x \in L$<br />
  → $\exists u\in\{0,1\}^n . M(x,u) =1$<br />
  → existe una traza que representa la corrida exitosa de $M(x,u)$<br />
  → usando $u$ y la traza, se puede construir una asignacion $z$ tal que $\varphi_x(z)=1$<br />
  → $\varphi_x \in$ CNF-SAT
* supongamos $\varphi_x \in$ CNF-SAT<br />
  → existe $z$, $\varphi_x(z) =1$<br />
  → constuir $u$ a partir de $z$<br />
  → x$\in L$ 

---

* L $\leq_p$ CNF-SAT, para cualquier L $\in$ **NP**
* Entonces CNF-SAT es **NP**-difícil
* Entonces CNF-SAT es **NP**-completo

# **3SAT**

* una formula es en 3FNC si es en FNC y cada cláusula tiene como máximo 3 literales
* **3SAT** = $\{ \lfloor \varphi \rfloor \mid \varphi$ es en 3FNC y es satisfacible $\}$

<div class="thm">
**CNF-SAT** $\leq_p$ **3SAT**
</div>

Idea: usar regla de la resolución al revés.

# Uso de la **NP** completitud de 3SAT

* Se sospecha que **P** ≠ **NP**, o por lo menos que estar **NP** difícil es una garantia de
  dificultad aceptada hoy en día
* Teniendo un lenguaje, es más fácil mostrar una reducción desde 3SAT que
  desde cualquier lenguaje **NP**
* La forma de formulas en FNC-3 es conveniente.

# INDSET es **NP** completo

INDSET =
  $\{\lfloor (G,k) \rfloor \mid\exists S\subseteq N(G),|S|\geq k,\forall uv\in S,uv\notin E(G)\}$

INDSET $\in$ **NP** porque el certificado es el conjunto $S$.

Mostramos 3SAT $\leq_p$ INDSET.

---

Sea $\varphi = \bigwedge C_i$ una formula en FNC3.
Sea $m$ el número de cláusulas de $\varphi$, construimos un grafo $G$ tal que
$\lfloor \varphi \rfloor \in 3SAT \leftrightarrow \lfloor (G,m) \rfloor \in INDSET$.

Cada cláusula $C$ de $\varphi$ tiene ≤ 3 literales, entonces
hay ≤ 7 asignaciones que la satisfacen.

Construimos para $C$ un cluster de 7 nodos, marquamos cada uno con
las asignaciones que satisfacen su cluster.

Conectamos dos puntos si pertenecen a clusters distintos y representan asignaciones
inconsistentes (ie, 1 variables es asignada a 1 en un nodo y 0
en el otro).

Conectamos entre si todos los puntos de un mismo cluster.

---

<img src="3SAT_INDSET.png" />

---

> * La transformación se puede hacer en tiempo polinomial.
> * $\varphi$ satisfacible<br />→ existe asignación $z$ tal que $\varphi(z)=1$<br />
>   → definir $S$ los nodos de cada cluster que corresponden a la restriccion de $z$
    a las variables del cluster<br /> → hay $m$ nodos no conectados entre si en $G$
> * hay $m$ nodos no conectados entre si en $G$<br />
>   → pertenecen a clusters distintos<br />
>   → las asignaciones parciales que esos nodos representan son consistentes<br />
>   → con ellas construimos $z$ tal que $\varphi(z)=1$<br />
>   → $\varphi$ satisfacible

# VERTEXCOVER es **NP** completo

Reducción descrita en Sipser, teorema 7.44.

# Búsqueda vs decisión

<div class="thm">
Supongamos **P** = **NP**. Para todo lenguaje L $\in$ **NP** y una verificadora
M para L, existe una MT B corriendo en tiempo polinomial que construye un certificado
para todo input $x \in L$.
</div>

---

Demostración:

* en el caso de SAT: si **NP**=**P**, construir una asignación para $\varphi$ se hace en tiempo polinomial:
    1. probar si $\varphi\in$ SAT, seguir si es el caso
    1. probar si $\varphi(x_1 ← 0) \in$ SAT, en ese caso fijar el primer bit de $z$
       a $0$ y reemplazar $x_1$ por 0 en $\varphi$, en el caso contrario hacer lo mismo con $1$
    1. seguir hasta tener una asignación completa para $\varphi$
* en el caso general, con L $\in$ **NP**, usar el hecho que la reducción que vimos es una reducción
  *Levin*, ie, se puede construir un certificado para $x\in L$ a partir del certificado de $\varphi_x$
  en tiempo polinomial.

# **P** vs **NP** y **NP** completitud

> * 1956: Kurt Gödel escribe una carta a John von Neumann planteándole
>   la posibilidad de decidir en tiempo polinomial si una formula
>   de primer order tiene una prueba de longitud $n$
> * 1971: Cook "The Complexity of Theorem Proving Procedures"
> * 1972: Karp "Reducibility Among Combinatorial Problems"
> * 1972: Miller and Thatcher, editors.
>   "Complexity of computer computations". Plenum Press:
>   proceedings de la conferencia donde se presentó el artículo de Karp
>   frente a 200 investigadores en computación (según el deseo de
>   Michael Rabin)
> * 1971/3: Levin "Универсальные задачи перебора" (*Universal search problems*)

# Como manejar problemas **NP**-completos

> * Heuristicas: NP es acerca de todos los inputs posibles, mientras que en casos
>   concretos los inputs pueden tener rasgos explotables
>   (simplicidad, simetrías, variables interconectadas, etc.)
> * Fijar parametros (parametrized complexity)
> * Canjear exactitud por velocidad

# **EXP** vs **NEXP**

<div class="thm">
**NEXP** = $\bigcup_{c \geq 0}$**NTIME**$(2^{n^c})$.
</div>

Ejercicio: encontrar la definición equivalente con certificados.

Teorema:

<div class="thm">
**EXP** ≠ **NEXP** implica **P** ≠ **NP**
</div>

---

Supongamos **P**=**NP**.
Sea L$\in$**NTIME**$(2^{n^c})$, y M una MT para $L$.

Sea $L_{pad}=\{ x 0 1^{2^{|x|}^c}  \mid x \in L \}$.
Mostramos que $L_{pad}\in$ **NP**:

* existe una MTND para $L_{pad}$: dado $y$, chequea si es de la forma
  $y = z 01^{2^{|z|}^c$. Si no, output 0.
* Correr $M$ con input $z$ por $2^{|z|}^c$ pasos.
* El tiempo de corrida es polinomial en función de $|y|$.

Entonces si **P**=**NP**, $L_{pad}\in$**P** y existe $M'$ deterministica
que decide $L_{pad}$.

Entonces $L\in$**EXP**: para decidir si $x \in$**L**, lo padeamos
con $01^{2^{|x|^c}}$ y vemos si esto pertenece a $L_{pad}$ usando $M'$.

# El problema de la factorización

Existen problemas naturales
que se sospecha estar en **NP** $\setminus$ **P** sin ser **NP** completos.

Factorización:

> Dado $n, m\in\mathbb{N}$ tal que $1 \leq m \leq n$, ¿ $n$ tiene un factor $d$
> tal que $1 < d < m$?

Se sospecha que FACTOR no está en **P**, ni es **NP** completo ni **coNP** completo.

Observación: el problema ¿ $n\in\mathbb{N}$ es un número compuesto? es una versión
relajada de FACTOR y está en **P** (equivalentemente, PRIMES está en **P**).

---

Otros problemas en **NP**, sospechados de estar en
**NP** $\setminus$ (**P** $\cup$ **NPC**)

>* isomorfirmo de grafos<br />
>  <img src="Graph_isomorphism_a.svg" height="200" />  <img src="Graph_isomorphism_b.svg" width="200"/>
>* problema de la autopista con peaje (*turnpike problem*):
>  dadas $n(n-1)/2$ distancias entre pares de puntos,
>  ¿les corresponde alguna configuración de $n$ puntos en una línea?


# Teorema de Ladner

<div class="thm">
Si **P**≠**NP**, existe un lenguaje en **NP** $\setminus$ **P** que no es **NP** completo.
</div>

Idea: definir un problema tal que para ciertos inputs de tamaño $n$,
el problema es resolver SAT (para que no este en **P**), y en otros
inputs no hacer nada (para que no sea **NP** completo).

[Prueba detallada adaptada de Arora y Barak](ladner.html)

# Referencias

* Lipton "The P=NP Question and Gödel’s Lost Letter" (Cap. 1)
* [Unos problemas entre **P** y **NP** completitud](http://cstheory.stackexchange.com/questions/79/problems-between-p-and-npc/237#237)
