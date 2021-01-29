We want to show that $L \leq_{p} SAT$ for any $L \in NP$.

Let $L \in NP$. By definition, there is polynomial time TM M and some polynomial
$p: \mathbb{N} \mapsto \mathbb{N}$ such that for
every $x \in \{0,1\}^*$: $$x \in L \leftrightarrow  \exists u \in \{0,1\}^{p(|x|)} . M(x,u)=1$$

We want a polynomial-time transformation $x \mapsto \varphi_x$
to CNF formulas such that:
$$x \in L \leftrightarrow \exists u \in \{0,1\}^{p(|x|)} . M(x,u)=1 \leftrightarrow \varphi_x \in SAT$$

We can replace the verifier M (with an acceptable polynomial-time slowdown)
with a version that:

1. only has two tapes (an input tape and a work/output tape)
2. is oblivious, ie, whose head movement does not depend on the contents of its tapes.
   That is, M’s computation takes the same time for all inputs of size
   $n$, and for every $i$ the location of M’s heads at the $i$th step depends only on $i$ and the
   length of the input.

Let $Q$ the set of M's states and $\Gamma$ its alphabet. The *snapshot* of
M's execution on some input $y$ at a particular step $i$ is the triple
$(a, b, q) \in \Gamma \times \Gamma \times Q$ such that $a, b$ are the symbols under
M's heads and $q$ is the state M is in at the $i$th step.
The snapshot can be encoded as a binary string of length $c$, a constant depending upon |Q| and $|\Gamma|$.

The *trace* of M's execucion on some input $y$ is the succession of $p'(|x|)$ snapshots.

Now, suppose somebody were to claim the existence of some $u$ satisfying
$M(x,u) = 1$ and, as evidence, present you with a trace of M's execution on $x,u$.
How can you tell that the trace present a valid computation that was actually performed
by M?

<!-- Let $\delta : \Gamma^2 \times Q \mapsto \Gamma \times Q \times \{←,=,→\}^2$
M's transition function. -->
From M's transition function $\delta$, define the functions:

* $\delta_{write}: \Gamma^2 \times Q \mapsto \Gamma$
* $\delta_{state}: \Gamma^2 \times Q \mapsto Q$

Since M is oblivious, we can define the following two functions:

* $inpos: \mathbb{N}\mapsto\mathbb{N}$, $inpos(n)$ is
  the location of M's input tape head at step $n$.
* $prev: \mathbb{N}\mapsto\mathbb{N}$, $prev(n)$ is
  the last step before $n$ when M's head was in the same cell on its work
  tape that at step $n$. If there is no such step,
  we define $prev(n)=1$.

Let us use $y$ as shorthand for $x u$.
We have the following constraints for a snapshot $z_i =(a_i,b_i,q_i)$:

* $a_i = y[inpos(i)]$
* $b_i = \delta_{write}(z_{prev(i)})$
* $q_i = \delta_{state}(z_{i-1})$

Because M is oblivious, the values $inpos(i)$ and $prev(i)$ do not depend on
the particular input $y$. Also, these indices can be computed in
polynomial time by simulating M on a trivial input, but it is not necessary to them
to show that the polynomial-time translation exists.

Now we turn the above thought exercise into a reduction. Recall that
an input $x \in \{0, 1\}^n$ is in L if and only if $M(x \circ  u) = 1$
for some $u \in \{0, 1\}^{p(n)}$.
The previous discussion shows this latter condition occurs if and only if there exists a string
$y \in \{0, 1\}^{n+p(n)}$ and a sequence of strings $z_1, \ldots, z_{T(n)} \in \{0, 1\}^c$
(where T(n) is the number of steps M takes on inputs of length n + p(n)) satisfying
the following four conditions:

1. The first $n$ bits of $y$ are equal to $x$
2. The string $z_1$ encodes the initial snapshot of M,
   $(\triangleright, \triangleright, q_{start})$.
3. The last string $z_{T(n)}$ encodes a snapshot in which the machine halts and outputs 1, that is,
   we have $b_{T(n)}=1$ and $q_{T(n)} = q_{halt}$.
4. For every $i \in \{2, \ldots , T(n) \}$:
       * $a_i = y_{inpos(i)}$
       * $b_i = \delta_{write}(z_{prev(i)})$
       * $q_i = \delta_{state}(z_{i-1})$

4\. can be rephrased as:
there is a function $f$ such that $z_i = f(z_{prev(i)},z_{i-1},y_{inpos(i)})$.
And further as: there is a binary function $f'$ such that
$f'(z_{prev(i)},z_{i-1},y_{inpos(i)}, z_i) = 1$
iff $z_i = f(z_{prev(i)},z_{i-1},y_{inpos(i)})$.
Hence the last condition can be stated as:

4. For every $i \in \{2, \ldots , T(n) \}$:
   <br />$f'(z_{prev(i)},z_{i-1},y_{inpos(i)}, z_i) = 1$

Now we define a propositional formula $\varphi_x$ that takes variables $Y_i$
for i from 1 to n+p(n), and $Z_i$ for i from 1 to $c T(n)$. Note that a
snapshot $z_i$ is encoded as $c$ propositional symbols $Z_j$.
$\varphi_x$ will verify that the variables $Y_i$ and $Z_i$ verify the four
conditions above.

Condition 1 can be expressed as a CNF formula of size $2n$:
$$(y_1 \vee \neg x_1) \wedge (\neg y_1 \vee x_1) \wedge \ldots
  \wedge (y_n \vee \neg x_n) \wedge (\neg y_n \vee x_n)$$

Similarly, Condition 2 and 3 can respectively be expressed as a CNF formula
of size $2c$ and of size $\leq 2c$.

Each $(T(n)-1)$ condition of Condition 4 is a formula depending on at most
$3c+1$ variables (since $f': \{0,1\}^{3c +1} \mapsto \{0,1\}$).
One condition can be expressed as a CNF formula of size at most
$(3c + 1)2^{3c+1}$.

The AND of these formulas is a CNF formula of size $d(n + T(n))$ where $d$ is some constant
depending only on M. Moreover, this CNF formula can be computed in time
polynomial in the running time of M (remember we have to compute the indices
$prev$ and $inpos$ with a dummy input).

Thus $x \in L \leftrightarrow \varphi_x \in SAT$ with $\varphi_x$ a CNF formula
of computable in time polynomial in function of $|x|$.

