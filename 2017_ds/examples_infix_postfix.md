in: 3 + 5

out: 3 5 +

S:
S:+
S:

----

in: 7 - 3 + 8

out: 7 3 + -

S:
S: -
S: - +
pop everything -> S:

----
in:7 + 3 * 4
out: 7 3 4 * -

S
S +
S + *
S + *
pop everything
----
in: (7 + 3) * 4
out: 7 3 + 4

S
S ( +
S *
S
----
in: (7+3)/(8-2*3)
out: 73+823*-/

S (+
S / ( - *
S /
S
----
in: (7-8/2/2)*((7-2)*3-6)
out: 7 8 2 2 / / - 7 2 - 3 * 6 - *

S ( - / /
S * ( ( -
S * ( * 
S * ( -
S *
S


