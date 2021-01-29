% Complejidad Computacional
  Semana 5: **NL**, **L**

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

Un *transductor logspace* es una MT con:

* una cinta de input en lectura sola
* una cinta de trabajo de tamaño $O(log(n))$
* una cinta de output en *escritura sola*<br />
  (su cabezal puede sólo quedarse inactivo
  o escribir un símbolo y moverse a la derecha)

Un transductor logspace $M$ computa una función 
$f:\{0, 1\}^* \mapsto \{0,1\}^*$ si para todo input $x$,
$M$ se detiene con $f(x)$  escrito
en la cinta de output.

Decimos que $f$ es calculable en espacio logarítmico.

</div>

# Reducciónes logarítmicas

<div class="thm">
Para dos lenguajes $B,C$, si existe una función $f$ calculable
en espacio logarítmico tal que para todo $x\in\{0,1\}^*$,
$x\in B$ ssi $f(x)\in C$,
entonces decimos que $B$ es *reducible en espacio logarímico* a $C$
(escrito $B \leq_L C$).

$C\in$**NL** es **NL** completo si para todo
$B \in$ **NL**, $B \leq_L C$.

</div>

# Composición en espacio log

Propiedades
<div class="thm">
Si $B \leq_L C$ y $C \in$ **L** entonces $B\in$ **L**.

</div>

> * Mostramos que existe $M_B$ que decide $B$ en espacio log.
> * Primer intento: pegar el transductor $T$ de $B \leq_L C$
>   con la máquina $M_C$ que decide $C$.
> * Pero eso necesita $f(x)$,
>   que puede tener un tamaño más grande que logarítmico
>   en función de $B$.

---

Lemma:
<div class="thm">
Si $T$ es un transductor logspace que computa $x \mapsto f(x)$,
entonces se puede modificar en $T'$ que computa
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

> * Modificar $T$ para que compute sólo el bit de $f(x)$ que
>   $M_C$ necesita (entonces su input tiene forma
>   $\lfloor x, i \rfloor$)
> * $M_B$ controla donde está el cabezal de lectura de $M_C$
>   en $f(x)$, y corre el cálculo de $f(x)_i$ por $T$
> * Hacer eso necesita calcular de vuelta varias partes de $f(x)$,
>   entonces es ineficiente en tiempo, pero es eficiente en espacio.
> * $M_B$ corre en espacio
>   $O( log \left| f(x) \right|)$ + $O(s(\left|x\right|))$ + $O(s'(\left|f(x)\right|))$,
>   con $s$ y $s'$ el espacio de $T$ y $M_C$
> * entonces $M_B$ corre en espacio $O(log\left|x\right|)$


# PATH es **NL** completo

> * sea $B\in$**NL**, mostramos $B\leq_L PATH$
> * $M$ es nondeterminística y decide $B$ en espacio $O(log(n))$
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
$B\in$**NL** si existe una MT $M$ determinística con
una cinta adicional en *lectura única*, y un polinomio
$p:\mathbb{N}\mapsto\mathbb{N}$ tal que para todo $x\in\{0,1\}^*$:

$x\in B \leftrightarrow
   \exists u \in \{0,1\}^{p(\left| x \right|)} s.t. M(x,u)=1$

Con:

* $M(x,u)$ el output de $M$ con $x$ puesto en su cinta de input
  y $u$ puesto en su cinta en lectura única
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
El complemento de $PATH$ está en **NL**

</div>

* Immerman. *Nondeterministic space is closed under complementation*, 1988
* Szelepcsényi. *The method of forcing for nondeterministic automata*, 1987

# Demostración

Mostramos que hay un algoritmo A corriendo en espacio $O(log (n))$ tal que:

$A(G,s,t,u)=1$ ssi $t$ no es alcanzable desde $s$ en $G$, con certificado $u$
en lectura única.

Llamamos:

* $n$ el número de nodos de $G$
* $C_i$ el conjunto de nodos alcanzables desde $s$ en $\leq i$ pasos
* $c_i$ el tamaño de $C_i$

---

Para cualquier input, A ya sabe:

$C_0 = \{s \}$

$c_0=1$

$v \in C_i$ puede ser chequeado facilmente:
un certificado $path_i(s,v)$ en lectura única es
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
2. los nodos están listados en órden creciente
3. ningun de los nodos listados es $v$ ni un vecino de $v$
4. cada certificado $path_{i-1}(s,v_j)$ es correcto

en espacio $O(log(n))$ con certificado en lectura única.

---

Certificado $size_i(k)$, asumiendo $c_{i-1}$ está conocido:

> $v_1, (no)Path_i(s,v_1), v_2,(no)Path_i(s,v_2),\ldots, v_n, (no)Path_i(s,v_n)$

dependiendo de si $v\in C_i$ o no.

Se puede chequear que

1. los nodos están listados en órden creciente
2. cada certificado $path_i(s,v)$ o $noPath_i(s,v)$ es correcto
3. el número de nodos en $C_i$ es exactamente $k$

en espacio log. con certificado en lectura única.

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

* Arora y Barak. Capítulo 4
* [Matthew Johnson, Lectures Notes 2009](http://www.dur.ac.uk/matthew.johnson2/teaching/acc/lectures/lecture12handout.pdf)
* [Nabil Mustafa, Lecture Notes, 2008](http://russell.lums.edu.pk/~cs514s07/slides/lecture12.pdf)
* Savitch. *Relationships between nondeterministic and deterministic tape
  complexities*. J. Comput. Syst. Sci., 4:177–192, 1970.
* Immerman. *Nondeterministic space is closed under complementation*. SIAM
  J. Comput., 17(5):935–938, 1988.
* Szelepcsényi. *The method of forcing for nondeterministic automata*. Bulletin
  of the European Association for Theoretical Computer Science, 33:96–100, Oct. 1987.
  Technical Contributions.



