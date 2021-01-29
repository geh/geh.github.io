% MD3-computabilidad, parcial 1
% 2019-10-16

No se pueden consultar apuntes, buscar por internet o comunicar con otra gente.

Se entregan las respuestas por escrito sobre papel.

Duración del parcial: 1 hora.

# 1. Mostrar que un lenguaje es decidible (3 puntos)

Mostrar que el lenguaje binario siguiente es decidible:

$L = \{ 1^n01^n \mid n > 0 \}$

Por ejemplo las palabras siguientes pertenecen a $L$:

* $101$
* $11011$
* $11111011111$

Las siguientes no pertencen a $L$:

* $100$
* $001$
* $1111$
* la palabra vacía

Describir una máquina de Turing que lo decida.

# 2. Mostrar que un lenguaje pertenece a **P** (2 puntos)

Mostrar que el lenguaje siguiente pertenece a la clase **P**:

$S = \{ (n,m,p) \mid n,m,p \in \mathbb{N}, n = m * p \}$

# 3. Mostrar que un lenguaje pertenece a **NP** (3 puntos)

El problema de la autopista con peajes (*turnpike problem*) es el siguiente:

Tenemos $n$ puntos que debemos colocar en una línea recta (o $n$ peajes a colocar
en una autopista).

Tenemos una lista de $n(n - 1) / 2$ distancias entre cada par de puntos (distintos),
sin saber a qué puntos se corresponden esas distancias.

¿Es posible colocar esos $n$ puntos en una línea respetando esas distancias?

Explicar por qué el lenguaje que corresponde a este problema está en **NP**.

# 4. Reducción polinomial entre dos lenguajes (2 puntos)

**CircuitSAT** es el lenguaje de los *circuitos Booleanos* satisfactibles.

Un circuito Boolean es un objeto donde empezemos con $n$ variables Booleanas $x_1, x_2, ... x_n$
que son la "entrada".

Luego podemos definir variables nuevas que son el AND, OR, o NOT de variables que ya fueron
definidas previamente. Por ejemplo:

  * $x_{n+1}$ := $x_3$ or $x_n$
  * $x_{n+2}$ := not($x_{n+1}$)
  * $x_{n+3}$ := $x_1$ and $x_{n+2}$
  * ... 

La última variable de esta lista es la "salida".

Entonces un circuito es satisfactible si existe una forma de asignar valores 0 o 1 (o "falso" o "verdadero")
a las entradas para que la salida se evalúe a 1 (o "verdadero").

**SAT** es el lenguaje de las formulas Booleanas satisfactibles. Esas formulas se escriben como conjuntos de cláusulas,
y cada cláusula es una disyunción.

Afirmamos lo siguiente: si podemos decidir **SAT**, tambien podemos decidir **CircuitSAT**.

¿Por qué? ¡Basicamente cada instancia de **CircuitSAT** es una instancia de **SAT** disfrazada!

Cada vez que calculamos un AND, OR o NOT, estamos relacionando una variable nueva con una o dos
viejas. Y cada una de estas relaciones se puede expresar con un conjunto de cláusulas.

Por ejemplo:

  * $x_{n+1}$ := $x_3$ or $x_n$

Se vuelve:

  * $x_{n+1}$ or not($x_3$)
  * $x_{n+1}$ or not($x_n$)
  * not($x_{n+1}$) or $x_3$ or $x_n$. 


* ¿Porqué tenemos una reducción polinomial entre estos dos lenguajes?
* ¿En qué dirección? Escribir la relación.



