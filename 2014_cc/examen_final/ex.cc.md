% Examen "take home" Complejidad Computacional 2014
% 2016-06-27

 1. Busy Beaver.
   
    a. ¿Qué es la competencia del Busy Beaver? Ver "Busy Beaver"
    en wikipedia. Resumir en 1 párrafo.
    Considerar la competencia en cantidad de pasos ejecutados por
    la máquina, no en cantidad de símbolos escritos.
   
    b. Considerar las MT de 1 cinta, 2 símbolos (0 y 1),
    y 2 estados. Qué tipo tiene la función de transición de
    esas máquinas? Cúantas MT de 1 cinta, 2 símbolos y 2 estados
    existen?
   
    c. Enumerar todas las máquinas de 1 cinta, 2 símbolos y 2 estados.
    Algunas de ellas son "equivalentes", si consideramos que arrancan
    con una cinta vacía?
   
    d. Entre las máquinas enumeradas, dar 2 ejemplos de máquinas
    cuya ejecución con cinta inicial vacía no termina (y decir porqué).
    Dar la máquina "campeona" de esta categoria para la competencia
    del Busy Beaver y mostrar toda su ejecución con una cinta inicial
    vacía.

 2. Sea L un lenguaje decidible. Sea U el lenguaje unario tal que
    la string i pertenece a U si, y solamente si, existe una string
    j que pertenece a L, tal que |j|=|i|. Mostrar que U es decidible.

 3. Considerar los problemas SAT y CNF-SAT y 3SAT como definidos en el
    apunte. Demostrar que $SAT \leq_p$ CNF-SAT y CNF-SAT $\leq_p$ 3SAT.

 4. Sea X una clase de complejidad. Dar una definición de X-completitud
    con reducciones P (en tiempo polinomial).
    Demostrar que con esa definición, todos los problemas en P son
    P-completos.
    ¿Qué otro tipo de reducciones existen para definir la classe de
    problemas P-completos? ¿Porqué son más interesantes?
   
