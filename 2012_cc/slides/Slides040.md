% Complejidad Computacional
  Semana 4: Complejidad espacial

# Contenido

* cálculos acotados por espacio
* clases PSPACE, NPSPACE
* relación con P, NP, EXPTIME
* problemas completos para clases espaciales
* clases espaciales sublineales (L, NL)

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
nondeterminísticas cumplen con el mismo límite de espacio.

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

En cada paso, una MT determinística puede descubrir cómo máximo
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

Sea M una MT determinística o nondeterminística, $x\in\{0,1\}^*$.

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

Sea $n$ la cantidad de nodos en el grafo:

* tamaño de un puntero para referirse a un nodo: $log(n)$
* tamaño de un contador para referirse a los sucesores de un
  nodo que ya encontramos: $log(n)$
* tamaño de un contador para guardar la longitud del camino
  visitado: $log(n)$
* explorar todos los caminos de longitud $< n$ posibles
  a partir de $s$, pasar al estado $q_{accept}$ si
  encontramos $t$

# **PSPACE** completitud

<div class="thm">
Un lenguaje $L$ es **PSPACE** hard si para todo
$L' \in$ **PSPACE**, $L' \leq_p L$.

Si además $L \in$ **PSPACE**, $L$ es **PSPACE** completo.

</div>

Usamos las mismas reducciones que para **NP**, ie,
$f$ corriendo en tiempo polinomial.

# Un lenguaje **PSPACE** completo

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
> * $\psi_i(C,C') =$<br />
>   $\exists C'' ~ \forall D ~ \forall E ~ 
>   ( (D=C \wedge E=C'') \vee (D=C'' \wedge E=C') )$<br />
>   $\rightarrow \psi_{i-1}(D,E)$
> * $|\psi_i| \leq |\psi_{i-1}| + O(m)$
> * $|\psi_m| \leq O(m^2)$
> * ¡Convertimos $\psi_m$ en forma prenexa en tiempo polinomial!

# Observación

* la demostración anda para $L\in$**NPSPACE**
* entonces **PSPACE**=**NPSPACE**


Teorema de Savitch (1970):

<div class="thm">
Para $s:\mathbb{N}\mapsto\mathbb{N}$ constructible en espacio:

**NSPACE**$(s(n))$ $\subseteq$ **SPACE**$(s(n)^2)$

</div>

# Referencias

* Arora y Barak Sección 4.2.2
* Stearns, Hartmanis y Lewis. *Hierarchies of memory limited
  computations*. In FOCS, pages 179–190. IEEE, 1965.
* Sipser Sección 9.1: Space hierarchy theorem
* Stockmeyer y Meyer. *Word problems requiring exponential time*.
  In STOC, pages 1–9. ACM, 1973.
* Savitch. *Relationships between nondeterministic and deterministic tape
  complexities*. J. Comput. Syst. Sci., 4:177–192, 1970.
