% Teorema de Ladner

# Enunciado

Asumamos **P**≠**NP**.
Entonces existe un lenguaje $L\in$**NP**$\setminus$**P**
que no es **NP** completo.

# Preliminarios

Dado $i\in\mathbb{N}$, llamamos $M_i$ la máquina de Turing
representada por la representación binaria de la string $\lfloor i \rfloor$.
Si esta string no representa ninguna MT, mapeamos $i$ a una máquina
que no hace nada.

Una máquina está representada por una cantidad infinita de strings.


# Demostración

Para toda función  $H: \mathbb{N} \mapsto \mathbb{N}$, definimos
$$SAT_H = \{ \varphi 0 1^{n^{H(n)}} \mid \varphi \in SAT, n = |\varphi|\}$$

Ahora, definimos $H: \mathbb{N} \mapsto \mathbb{N}$ de la manera siguiente:

H(0) = H(1) = 0

H(n) es el $i < log log n$ más chiquito tal que para todo input
$x$ de tamaño $\leq log n$, $M_i$ sale $SAT_H(x)$ en $i|x|^i$ pasos.
Si no hay tal $i$, ponemos $H(n)=log log n$.

* **Claim** $H$ está bien definida:
  $H(n)$ sirve para definir las strings de $SAT_H$ de longitud superior a $n$
  (por definición de $SAT_H$), mientras que la definición de $H$ depende
  de chequear strings de longitud $log n$ cómo máximo.

* **Claim** se puede computar $H(n)$ en $O(n^3)$ pasos: ejercicio.

* **Claim**: $SAT_H \in$ **P** ssi $\forall n \exists C$. $H(n)\leq C$
    * (←): si $H(n)\leq C$, $H(n)$ sólo puede tomar un número
      finito de valores
      <br />→ existe $i$ tal que $H(n)=i$ para un número infinito de $n$
      <br />→ $M_i$ decide $SAT_H$ en tiempo $i.n^i$
      (si no, $\exists x$ tq $M_i(x)$ no tiene ouput correcto en tiempo $i.n^i$,
      entonces, $\forall n > 2^{|x|}$, $H(n)≠i$)
      <br />**Observación** esto es cierto también si sólo asumimos que
      existe una constante $C$ tal que $H(n)\leq C$ para una cantidad infinita
      de $n$, entonces tenemos: si $SAT_H \notin$ **P** entonces
      $\lim H(n) = +\infty$
    * (→): supongamos M decide $SAT_H$ en  $c.n^c$ pasos. Cómo existe una cantidad
      infinita de strings que representa $M$, existe un $i > c$ tal que $M=M_i$
      (entonces $M$ corre en $i.n^i$ pasos).
      <br />Entonces para $n \geq 2^{2^i}$ tenemos $H(n)\leq i$.

Con el último Claim, mostramos que si **P**≠**NP**, $SAT_H$ no está en **P**
ni es **NP** completo.

Supongamos $SAT_H \in$ **P**. Entonces $\forall n \exists C . H(n)\leq C$.
Podemos construir una reducción polinomial $f$ de SAT a $SAT_H$ de tal
manera: sea $x$, computamos $H(|x|)$ (en tiempo polinomial), generamos la
string $01^{|x|^{H(|x|)}}$ (de longitud $1 + |x|^C$ polinomial).
Entonces SAT$\in$**P** y **P** = **NP**
$\otimes$.


Ahora supongamos $SAT_H$ es **NP** completo.

Entonces existe una reducción $f$ de SAT a $SAT_H$ corriendo
en tiempo $O(n^d)$ con $d$ constante.
Entonces $|f(x)| \leq |x|^d$ para cualquier $x$

Dado que $SAT_H$ no está en **P**, tenemos $\lim H(n) = +\infty$
$f$ corre en tiempo polinomial entonces existe un $n_0$ tal que
$f$ mapea instancias de SAT de tamaño $n > n_0$ a
instancias de $SAT_H$ de tamaño $< n^{H(n)}$. $n^{H(n)}$ no
es acotado por ningun polinomio.

Describimos un algoritmo para decidir SAT usando $f$:
Para input $|\varphi| < n_0$, codificamos en duro las respuestas.
Para input $|\varphi| \geq  n_0$:

* construir $f(\varphi)$
* si $f(\varphi)$ no es de la forma $\psi 0 1^{|\psi|^{H(\psi)}}$,
  return UNSAT
  (chequeo en tiempo poli.: medir $|\psi|$, computar $H(|\psi|)$ y $|\psi|^{H(|\psi|)}$),
  sino seguir
* correr el algoritmo sobre $\psi$

* **Claim** Para cada llamada recursiva del algoritmo, el tamaño del input
  decrece.<br />
  Demo. por el absurdo: asumamos $|x|\geq n_0$,
  $f(x) = \psi_x 0 1^{|\psi_x|^{H(\psi_x)}}$ y $|\psi_x| \geq |x|$ (el input no decrece).
  Tendríamos entonces $|\psi_x| \geq n_0$ y <br />
  $|x|+1+|x|^d \leq |\psi_x|+1+|\psi_x|^{H(|\psi_x|)}
      \leq |x|^d$, contradicción.

Entonces esto es un algoritmo polinomial para decidir SAT, entonces
**P**=**NP**, lo que contradice nuestra asumpción inicial.

