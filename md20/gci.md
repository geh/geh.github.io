# Gramáticas independientes del contexto

## Ejemplo


Gramática independiente del contexto para palíndromos:

1. P → ε
2. P → 0
3. P → 1
4. P → 0P0
5. P → 1P1

## Definición de las gramáticas independientes del contexto

Una GCI tiene 4 componentes:

1. *Alfabeto terminal*
2. *Variables*
3. Una de las variables es el *simbolo inicial*
4. Conjunto finito de *producciones* (o *reglas*)

Entonces $G = (V,T,P,S)$.

Ejemplo: $G_{PAL} = (\{P\}, \{0,1\}, A, P)$ donde $A$ es el conjunto
de reglas que vimos más arriba.

## Gramática independiente del contexto para expresiones simples

 1. E → I
 2. E → E + E
 3. $E → E * E$
 4. E → (E)
 5. E → a
 6. E → b
 7. E → Ia
 8. E → Ib
 9. E → I0
10. E → I1

## Derivaciones

Supongamos que $G = (V, T, P, S)$ es una GIC.

Sea $αAβ$ una
cadena de símbolos terminales y variables, siendo A una variable.

Es decir, $α$ y $β$ son cadenas de $(V ∪ T )^∗$ y$ $A$ pertenece a $V$.

Sea $A → γ$ una producción de $G$. Entonces decimos que $αAβ ⇒ αγβ$

Es decir, un paso de derivación reemplaza cualquier variable
de cualquier parte de la cadena por el cuerpo de una de sus producciones.

Podemos extender la relación ⇒ para representar cero, uno o más pasos de derivaciones.

Para las derivaciones utilizaremos el símbolo ∗ para indicar “cero o más pasos”.

# Ejemplo derivación

Que $a ∗ (a + b00)$ está en el lenguaje de la variable E se puede reflejar
en una derivación de dicha cadena, partiendo de la cadena E:

$E ⇒ E ∗ E ⇒ I ∗ E ⇒ a ∗ E ⇒$

$a ∗ (E) ⇒ a ∗ (E + E) ⇒ a ∗ (I + E) ⇒ a ∗ (a + E) ⇒$

$a ∗ (a + I) ⇒ a ∗ (a + I0) ⇒ a ∗ (a + I00) ⇒ a ∗ (a + b00)$

# Derivaciones izquierda y derecha

Con el fin de restringir el número de opciones disponibles en la derivación de una cadena, a menudo resulta útil
requerir que en cada paso se reemplace la variable más a la izquierda por uno de los cuerpos de sus producciones.

Tal derivación se conoce como derivación más a la izquierda, la cual se indica mediante las
relaciones $⇒_{lm}$ y $⇒^*_{lm}$.

También existe la derivación más a la derecha con el indice *rm* (rightmost).

# Lenguaje de una gramática

el lenguaje de G, designado como L(G), es el conjunto de cadenas terminales que
tienen derivaciones desde el símbolo inicial:

$L(G) = \{ w$ pertenece a $T^* | S \Rightarrow^* \}$

# Ejercicio 1

Diseñar gramáticas independientes del contexto para los siguientes lenguajes:

 a) El conjunto $\{0^n1^n | n ≥ 1}$, es decir, el conjunto de todas las cadenas formadas
    por uno o más ceros seguidos del mismo número de unos.
 b) El conjunto $\{a^ib^jc^k | i \neq j o j\neq k}$, es decir, el conjunto de cadenas formadas
    por letras a seguidas de letras b seguidas de letras c, tales que existe un número
    distinto de letras a que de letras b o un número distinto de letras b que de letras c,
    o ambos casos. 
 c) El conjunto de todas las cadenas formadas por letras a y letras b que no son de la
    forma $ww$, es decir, que no son iguales a ninguna cadena repetida.

# Ejercicio 2

La siguiente gramática genera el lenguaje representado por la expresión regular $0^∗1(0 + 1)^∗$ :

1. S → A1B
2. A → 0A | ε
3. S → 0B | 1B | ε

Obtenga las derivaciones más a la izquierda y más a la derecha de las siguientes cadenas:

 a) 00101.
 b) 1001.
 c) 00011.


# Ejercicio 3

Demuestre que todo lenguaje regular es un lenguaje independiente del contexto. Consejo:
construya una GIC por inducción sobre el número de operadores de la expresión regular.

# Árboles de derivación

Sea G = (V, T, P, S) una gramática. Los árboles de derivación para G son aquellos árboles que cumplen las
condiciones siguientes:

1. Cada nodo interior está etiquetado con una variable de V .

2. Cada hoja está etiquetada bien con una variable, un símbolo terminal o ε . Sin embargo, si la hoja está
etiquetada con ε , entonces tiene que ser el único hijo de su padre.

3. Si un nodo interior está etiquetado como A y sus hijos están etiquetados como:

$X_ , X_2 , ... , X_k$

respectivamente, comenzando por la izquierda, entonces $A → X_1 X_2 ··· X_k$ es una producción de P.
Observe que el único caso en que una de las X puede reemplazarse por ε es cuando es la etiqueta del único hijo y $A → ε$ es una producción de G.

# Aplicación de las GCI: analizadores sintácticos (parsers)

