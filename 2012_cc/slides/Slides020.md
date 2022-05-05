% Complejidad Computacional
  Semana 2: **NP**

# **P** vs **EXPTIME**

* **P** = $\bigcup_{c \geq 1}$ **TIME**$(n^c)$
* **EXPTIME** = $\bigcup_{c \geq 1}$**TIME**$(2^{n^c})$.
* time hierarchy theorem → **P** $\subset$ **EXPTIME**

---

* PATH = $\{\lfloor (G,s,t) \rfloor \mid$ G un grafo dirigido con camino $s \rightarrow t\}$
    * algoritmo brute-force ($m^m$ caminos posibles) → PATH $\in$ **2EXPTIME**
    * algoritmo goal-oriented → PATH $\in$ **P**   (Sipser, thm. 7.14)

---

* RELPRIME = $\{\lfloor (x,y) \rfloor \mid$ $x,y\in\mathbb{N}$ relativamente primos $\}$
    * enumerar todos los dividores de $x$ y $y$, si otro que $1$
      aparece en ambas listas, output $0$, sino $1$<br />
      → RELPRIME $\in$ **EXPTIME** si $\lfloor (x,y) \rfloor$ en binario!
    * algoritmo de Euclides: $\in$ **P**  (Sipser, thm. 7.15)

Algoritmo de Euclides:

    E: input: x,y
       hasta que y == 0:
           a) x ← x mod y
           b) intercambiar x y
       output x
    
    R: input: x,y
       si E(x,y) == 1 output accept
       si no, output reject       

---

* 2COLOR = $\{\lfloor G \rfloor \mid$ $G$ es coloreable por $2$ colores $\}$
    * $\in$ **P**
* 3COLOR = $\{\lfloor G \rfloor \mid$ $G$ es coloreable por $3$ colores $\}$
    * $\in$ **P**? no se sabe
    * $\in$ **EXPTIME** por cierto
    * $\in$ **TIME**($O(1.3289^n)$) (Beigel y Eppstein, 2000)


---

* VERTEXCOVER =<br />$\{ \lfloor (G,k) \rfloor \mid$ se puede "cubrir" el grafo $G$ con $k$ nodos $\}$
* $\in$ **EXPTIME**
* $\in$ **P**? no se sabe

<img src="Minimum-vertex-cover.svg" />
<!-- http://en.wikipedia.org/wiki/File:Minimum-vertex-cover.svg -->


---

LIN(0,1) = sistemas de inecuaciónes lineales con solución booleanas

(0-1 integer programming)

* sea $E=$:
    * $x_1 + 2x_2 + x_3 + x_4 \geq 3$<br />
      $x_1 + x_4 \geq 0$<br />
      $2x_1 + x_2 - x_3 \leq 1$
    * solución: $(1,1,0,0)$
    * E $\in$ LIN(0,1)

LIN(0,1) $\in$ **EXPTIME** (enumerar soluciones y verificar).

no se sabe si $\in$ **P**

---

* formulas de lógica proposicional:<br />
  $\varphi ~ :: x_i \mid \neg \varphi \mid \varphi_1 \wedge \varphi_2 \mid \varphi_1 \vee \varphi_2$
* sea $x_1, \ldots, x_n$ la variables que aparecen en $\varphi$,<br />
  escribimos $\varphi(z)$ el valor de $\varphi$ cuando sus variables son asignadas a $z \in \{0,1\}^n$
* si existe tal $z$, $\varphi$ es *satisfacible*
* SAT = $\{\lfloor \varphi \rfloor \mid$ $\varphi$ satisfacible $\}$
* $\in$ **EXPTIME**, no se sabe si $\in$ **P**
> si $\lfloor x \rfloor \in$ SAT, 3COLOR, ..., $x$ es un problema que tiene una *solución*,
> que es *pequeña* y *verificable rapidamente*

No es el caso de todos los lenguajes en **EXPTIME**.

# **NP**

<div class="thm">
Un lenguaje $L \subseteq \{0,1\}^*$ está en **NP** ssi existe un polinomio
$p: \mathbb{N} \mapsto \mathbb{N}$ y una MT M corriendo en tiempo polinomial
tal que para todo $x \in \{0,1\}^*$:

$~ ~ ~ x \in L \leftrightarrow \exists u \in \{0,1\}^{p(|x|)} ~$ tal que $~ M(x,u)=1$
</div>

> * M se llama la *verificadora* de $L$, y $u$ un *certificado* de $x$ (con respeto a $L$ y M).
> * La definición de **NP** es asimétrica!


# Lenguajes en **NP**

existencia de certificados cortos = pertenencia a **NP**:

* SAT, LIN(0,1), 3COLOR 
* traveling salesman
* subset sum
* graph isomorphism
* numeros compositos
* ...

# **P** $\subseteq$ **NP** $\subseteq$ **EXPTIME**

<div class="thm">
* **P** $\subseteq$ **NP**
* **NP** $\subseteq$ **EXPTIME**
</div>

<br />

> * No se sabe si **P** ≠ **NP**, ni si **NP** ≠ **EXPTIME**.


# **P** y **NP**

* en **P** están los problemas fáciles
* en **NP** están los problemas que tienen soluciones facilmente chequeables
* **NP** representa los problemas de búsqueda con soluciones cortas


# MMTT nondeterminísticas

> * Una MTND tiene $\delta_0$ and $\delta_1$ y un estado especial $q_{accept}$.
> * En cada paso, una MTND hace una elección arbitraria entre las dos funciones de transición.
> * Para un input $x$,  si existe **una** secuencia de
>   esos pasos (elecciones nondeterminísticas) que hace que
>   M alcanza $q_{accept}$, decimos que $M(x) = 1$.
> * Si *toda* secuencia de elecciones hace que M
>   se detiene sin alcanzar $q_{accept}$, decimos que $M(x)=0$.
> * M *corre en tiempo $T(n)$* si para todo input
>   $x\in\{0,1\}^*$ y toda secuencia de elecciones nondeterminísticas,
>   M alcanza $q_{halt}$ o $q_{accept}$ dentro de $T(|x|)$ pasos.

# Observación

Una MTND *no* representa cálculos fisicamente realisables.

# Definición tradicional de **NP**

<div class="thm">
Sea $T: \mathbb{N} \mapsto \mathbb{N}$ y $L\subseteq \{0,1\}^*$.
Decimos que $L \in$ **NTIME**$(T(n))$ si existe una constante $c>0$ y una TMND
M en tiempo $c⋅T(n)$ tal que para todo $x\in \{0,1\}^*$:
$$x\in L \leftrightarrow M(x)=1$$

$NP_{old} = \bigcup_{c\in \mathbb{N}}$**NTIME**$(n^c)$
</div>

<br />

# Equivalencia

<div class="thm">Las dos definiciones son equivalentes.</div>

1. $L \in NP_{old}$ → $L \in NP$
    * si existe una MTND M que decide $L$, se puede construir
      una MTD $M'$ que, con input $(x,u)$ y $u$ de longitud adecuada,
      simula una computación de $M$ con input $x$ eligiendo $\delta_{u[n]}$
      en cada paso $n$.
2. $L \in NP$ → $L \in NP_{old}$
    * se puede construir una MTND que, con input $x$, genera un certificado
      en $p(|x|)$ pasos de manera nondeterministica, y luego lo averigua
      con la verificadora de $L$.


# Filología computacional

> Travelling Salesman está en **NP** porque una gira satisfaciendo
> $l \leq k$ puede ser eligida nondeterministicalmente en $n$ pasos
> y luego la condicion $l \leq k$ verificada en un número polinomial
> de pasos [...].

> (John E. Savage, Models of Computation: Exploring the Power of Computing, 1997)



# Reducciones

<div class="thm">
Sea $A, B\subseteq\{0,1\}^*$. A es *(tiempo-)polinomialmente reducible* a
$B$, denotado $A \leq_{p} B$,
si existe una función $f:\{0,1\}^* \mapsto \{0,1\}^*$ calculable en tiempo polinomial
tal que para todo $x\in\{0,1\}^*$, $x \in A$ ssi $f(x)\in B$.
</div>

<br />

<div class="thm">
Proposiciones:

* si $A \leq_p B$ y $B \in$ **P** entonces $A \in$ **P**
* $\leq_p$ es transitiva
</div>

# Ejercicio

* Mostrar que 3COLOR $\leq_p$ SAT:
* Proveer una traducción G → $\varphi_G$
* Demostrar que: G tiene un coloreo → $\varphi_G$ tiene una asignación
* Demostrar que: $\varphi_G$ tiene una asignación → G tiene un coloreo

# **NP** hardness, completeness

<div class="thm">
Decimos que B es **NP** difícil (hard) si para todo $A\in$ **NP**, $A \leq_p B$.
Decimos que $B$ es **NP** completo (complete) si $B$ es
**NP** difícil y está en **NP**.
</div>

<br />

<div class="thm">
Proposiciones:

* si L es **NP** difícil y L $\in$ **P**, entonces **P** = **NP**
* si L es **NP** completo, entonces L $\in$ **P** ssi **P** = **NP**.
</div>

# Un lenguaje **NP** completo

TMSAT = $\{ \lfloor M, x, 1^n, 1^t \rfloor \mid \exists u\in\{0,1\}^n . M(x,u)=1$ en t pasos$\}$

>* TMSAT $\in$ **NP**: la verificadora de N es una máquina universal de Turing que simula
>  M con input (x,u) y verifica que su output es 1 despues de $t$ pasos. Su corrida es
>  polinomial en función de su input porque se puede simular máquinas con una desaceleración polinomial.
>* Sea L $\in$ **NP**. Existe verificadora M corriendo en tiempo polinomial $q(n)$, y existe
>  un polinomio $p(n)$ que determine el tamaño de los cartificados. Entonces a toda $x\inL$
>  le asociamos la string $\lfloor M, x, 1^{p(n)}, 1^{q(n+p(n))}\rfloor$.

# **CNF-SAT**

* un literal es una variable o una variable negada ($x_i$, $\neg x_i$)
* una cláusula es una disyuncción de literales tal que no aparece un literal y su contrario
* ej: $x_1 \vee \neg x_2 \vee \neg x_3$
* una formula proposicional es en Forma Normal Conjunctiva si
  es una conjuncción de cláusulas
* ej: $(x_1 \vee x_3) \wedge (x_1 \vee \neg x_2 \vee \neg x_3 \vee x_4) \wedge (\neg x_1 \vee \neg x_4)$
* definimos **CNF-SAT** = $\{ \lfloor \varphi \rfloor \mid \varphi$ es en FNC y es satisfacible $\}$

<div class="thm">
**SAT** $\leq_p$ **CNF-SAT**
</div>

* Idea: introducir variables nuevas para evitar explosión exponencial

# **3SAT**

* una formula es en 3FNC si es en FNC y cada cláusula tiene como máximo 3 literales
* ej: $(x_1 \vee x_3) \wedge (\neg x_2 \vee \neg x_3 \vee x_4) \wedge (\neg x_1 \vee \neg x_4)$
* **3SAT** = $\{ \lfloor \varphi \rfloor \mid \varphi$ es en 3FNC y es satisfacible $\}$

<div class="thm">
**CNF-SAT** $\leq_p$ **3SAT**
</div>

* Idea: introducir variables nuevas para evitar explosión exponencial

# Teorema de Cook-Levin

* Cook "The Complexity of Theorem Proving Procedures", 1971
* Levin "Универсальные задачи перебора", 1973

<div class="thm">
SAT es NP-complete
</div>

# Poder expresivo booleano

* Igualdad:
  la formula
  $$(x_1 \vee \neg y_1) \wedge (\neg x_1 \vee y_1)
  \wedge  \ldots \wedge (x_n \vee \neg y_n) \wedge (\neg x_n \vee y_n)$$
  es satisfacible por una asignación $z$ ssi $x_i(z) = y_i(z)$ para todo $i$.
* Funciónes booleanas:
  dada $f: \{0,1\}^k \mapsto \{0,1\}$:
    * para $v \in \{0,1\}^k$, definimos $C_v(x_i,..,x_k)$ una cláusula  tal que
      $C_v(v)=0$ y $C_v(u)=1$ para $u\neq v$
  <!--  * ej: $v = (1,1,0,1)$ → $\neg x_1 \vee \neg x_2 \vee x_3 \vee \neg x_4$-->
    * definimos
      $\varphi = \bigwedge_{\{v \mid f(v)=0\}} C_v(x_1,x_2,\ldots,x_k)$
    * entonces:
        * $\forall z \in \{0,1\}^k, \varphi(z) = f(z)$ 
        * $|\varphi| \leq k2^k$

---

Sea $L \in$ **NP**, queremos mostrar que $L \leq_{p} SAT$.

Por definición, $\exists$ M corriendo en tiempo polinomial y $p$ polinomio tal que para todo $x \in \{0,1\}^*$:
$$x \in L \leftrightarrow  \exists u \in \{0,1\}^{p(|x|)} . M(x,u)=1$$

Queremos una transformación en tiempo polinomial $x \mapsto \varphi_x$ tq:
$$\exists u \in \{0,1\}^{p(|x|)} . M(x,u)=1 \leftrightarrow \varphi_x \in SAT$$

---

Reemplazamos la verificadora M por una version que:

1. tiene 2 cintas (con input en lectura sola)
2. es indiferente:
    * las corridas de M toman el mismo tiempo para todo input de
      tamaño $n$
    * la ubicación de los cabezales de M en un paso $i$ sólo dependen
      del tamaño del input y de $i$

---

Un *instantáneo* de M es un tuple $(a,b,q) \in \Gamma^2 \times Q$.

Un instantáneo puede ser representado con $c$ bits, $c$ dependiendo de $\Gamma$ y $Q$
(y independiente del input).

Una *traza* es una succesión de instantáneos.

> * ¿Cuales son las condiciones que debe cumplir una traza para representar
>   una corrida exitosa de M con input $(x,u)$?
> * Vamos a construir $\varphi_x$ como un patrón de traza que es satisfacible
>   si y sólo si existe un $u$ tal que $M(x,u) =1$

---

A partir de la función de transición de M definimos:

* $\delta_{write}: \Gamma^2 \times Q \mapsto \Gamma$
  <!-- obs: delta_write(z1) deberia devolver blank, no lo hace pero se arregla -->
* $\delta_{state}: \Gamma^2 \times Q \mapsto Q$

Como M es indiferente, se pueden definir las funciones $\mathbb{N}\mapsto\mathbb{N}$:

* $inpos(i)$ la  posición del cabezal de input en el paso $i$.
* $prev(i)$ el último paso antes de $i$ tal que
  el cabezal de escritura está en el mismo lugar que en el paso $i$.
  Definimos $prev(i)=1$ por defecto.

Los valores de $inpos(i)$ and $prev(i)$ no dependen del input $y = (x,u)$.
Además esos valores pueden ser calculados en tiempo polinomial, corriendo M con un input trivial.

---

Restricciones que debe cumplir una traza $[z_1,z_2,...,z_{T(n)}]$
para representar una corrida exitosa de M con input $y$:

* $z_1= (\triangleright, \triangleright, q_{start})$
* $z_{T(n)} = ( a , 1 , q_{halt})$, $a \in \Gamma$
* para todo $z_i =(a_i,b_i,q_i)$ con $i\in \{2, \ldots , T(n) \}$:
    * $a_i = y_{inpos(i)}$
    * $b_i = \delta_{write}(z_{prev(i)})$
    * $q_i = \delta_{state}(z_{i-1})$
* para cada $i\in\{2, \ldots, T(n)\}$, existe une función $f$ tal que
  esas restricciones son cumplidas ssi
  $f(y_{inpos(i)},z_{prev(i)}, z_{i-1},z_i) = 1$

Queremos:
$\varphi_x \in SAT \leftrightarrow \exists u \in \{0,1\}^{p(|x|)} . y = (x,u) . M(y)=1$

---

Variables de $\varphi_x$:

* $Y_i$ con $i \in [1.. n + p(n)]$
* $Z_i$ con $i \in [1 .. cT(n)]$

Codificación de las restricciónes:

* $y[1..n] = x$ → formula de tamaño $2n$
* $z_1$ → formula de tamaño $2c$
* $z_{T(n)}$ → formula de tamaño $\leq 2c$

---

* para cada $i\in[2..T(n)]$,
  $$f(y_{inpos(i)},z_{prev(i)}, z_{i-1},z_i) = 1$$
  → formula $\varphi_i$ de tamaño $(3c + 1)2^{3c+1}$ tal que
  $$\varphi_i( Y', Z'_1, Z'_2, Z'_3  ) = 1 \leftrightarrow f(...) = 1$$,
  con:
    * $Y'$ son las variables que codifican $y_{inpos(i)}$
    * $Z'_1$ son las variables que codifican $z_{prev(i)}$
    * $Z'_2$ → $z_{i-1}$
    * $Z'_3$ → $z_i$
    * para cada $i$, para conocer $Y'$, $Z'_1$, $Z'_2$, $Z'_3$, hay que conocer $inpos(i)$
      y $prev(i)$, y para eso hay que correr y observar $M(0^{n + p(n)})$

---

Tamaño de $\varphi_x$:

$2n + 2c + 2c + (T(n)-1)(3c+1)2^{3c+1}$
$\leq$  $d(n + T(n))$, $d \in \mathbb{N}$

* se puede construir $x \mapsto \varphi_x$ en tiempo polinomial:
    1. correr $M(0^{n + p(n)})$
    2. generar $\varphi_x$
* $\varphi_x$ es en FNC

---

* supongamos $x \in L$<br />
  → $\exists u\in\{0,1\}^n . M(x,u) =1$<br />
  → existe una traza que reprensenta la corrida exitosa de $M(x,u)$<br />
  → usando $u$ y la traza, se puede construir una asignacion $z$ tal que $\varphi_x(z)=1$<br />
  → $\varphi_x \in$ CNF-SAT
* supongamos $\varphi_x \in$ CNF-SAT<br />
  → existe $z$, $\varphi_x(z) =1$<br />
  → constuir $u$ a partir de $z$<br />
  → x$\in L$ 

---

* L $\leq_p$ CNF-SAT
* Entonces CNF-SAT es **NP**-difícil
* Entonces es **NP**-completo

# Conclusiones

No tenemos ninguna demostración que **P** ≠ **NP** (≠ **EXPTIME**) pero hoy en día
se supone que **NP** es más dificil que **P** (y más fácil que **EXPTIME**).

<img src="pnpnphardnpcomplete.png" />

