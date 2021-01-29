% Complejidad computacional

Curso de posgrado
segundo semestre de 2014.

Guillaume Hoffmann, CONICET, Universidad Blas Pascal <guillaumh@gmail.com>

Horarios/lugar:

1. 25/09
2. 16/10?
3. 6/11?
4. 27/11?
5. 18/12?

Programa
--------
1. Modelos de cálculo, máquinas de Turing
    * automatas
    * circuitos booleanos
    * Máquina de Turing
    * MT universal
    * problema de la detención

2. Clases de complejidad
    * en tiempo: P, EXPTIME
    * en espacio: PSPACE, EXPSPACE
    * grafos de configuración
    * time hierarchy theorem
    * speedup (por una constante)
    * reducciones (levin vs otras)
    * reducciones logspace

3. P,NP
    * P vs NP
    * NP completitud
    * problema SAT
    * Teorema de Cook-Levin con circuitos booleanos
    * Oblivious Turing Machines (OTM)
    * Cook-Levin con OTM 

4. Complejidad espacial
    * PSPACE, NPSPACE. QBF, PSPACE completitud
    * L, NL, PATH, NL completitud, coNL
    * definicion certificate vs  non-determinism

5. Computación con randomness
    * RP, coRP, BPP, ZPP
    * Primes

6. (Bonus)
    * Problemas NP-intermedios/teorema de Ladner
    * Small Efficient Universal TM
    * Jerarquía polinomial
    * Aplicaciones a Criptografía
    * otros modelos (rule110)
    * complejidad y big data

Slides del 2012
---------------

* [Teórica 1](slides/Slides010.html)
* [Teórica 2](slides/Slides020.html)
* [Teórica 3](slides/Slides030.html)
* [Teórica 4](slides/Slides040.html)
* [Teórica 5](slides/Slides050.html)
* [Teórica 6: Polynomial Hierarchy (slides 1-85, apuntes de Nabil Mustafa)](http://sma.epfl.ch/~moustafa/Other/Complexityslides/lec13.pdf)
* Teórica 7:
    * [Randomized Computation (apuntes de Nabil Mustafa)](http://sma.epfl.ch/~moustafa/Other/Complexityslides/lec14.pdf)
    * [Randomized Computation 2, slides 1-35, Nabil Mustafa](http://sma.epfl.ch/~moustafa/Other/Complexityslides/lec15.pdf)
    * [UPATH in RL: sección 3.2 (Trapped in a maze), Scott Aaronson](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-045j-automata-computability-and-complexity-spring-2011/lecture-notes/MIT6_045JS11_lec11.pdf)
    * [Descripción del algoritmo de Miller y Rabin:
      (leer desde "As I mentioned"), Scott Aaronson](http://www.scottaaronson.com/democritus/lec7.html)
* Teórica 9:
    * [Crypto (apuntes de Scott Aaronson y Gus Gutoski)](http://www.scottaaronson.com/democritus/lec8.html)
* [Práctica 1](slides/Slides025.html)
* [Práctica 2](slides/Slides060.html)
* [Take Home 1](slides/takehome1.html)
* [Take Home 2](slides/takehome2.html)
* [Todas las slides](slides/)

Bibliografía
------------

* 'The annotated Turing', Petzold, 2008.
* 'Introduction To The Theory Of Computation', Sipser, 1996.
* ['Computational complexity', Papadimitriou](http://biblio.famaf.unc.edu.ar/cgi-bin/koha/opac-detail.pl?biblionumber=2946)

Más bibliografía
----------------

* 'Computational Complexity: A Modern Approach', Arora y Barak, 2007
    ([draft](http://www.cs.princeton.edu/theory/index.php/Compbook/Draft))
* ['Computational Complexity: A Modern Approach', Arora y Barak, 2007](http://biblio.famaf.unc.edu.ar/cgi-bin/koha/opac-detail.pl?biblionumber=16154)
* ['Introduction To The Theory Of Computation', Sipser (2nd ed)](http://biblio.famaf.unc.edu.ar/cgi-bin/koha/opac-detail.pl?biblionumber=15939)
* ['The P=NP Question and Gödel's Lost Letter', Lipton](http://biblio.famaf.unc.edu.ar/cgi-bin/koha/opac-detail.pl?biblionumber=16562)
* ['Computational complexity : a conceptual perspective', Goldreich](http://biblio.famaf.unc.edu.ar/cgi-bin/koha/opac-detail.pl?biblionumber=16564)
  [draft](http://www.wisdom.weizmann.ac.il/~oded/cc-drafts.html)

Videos
------

* [Una máquina de Turing tangible](http://www.aturingmachine.com/)<br />
  <iframe width="560" height="315" src="http://www.youtube.com/embed/WJ-ODmFjmrU?rel=0" frameborder="0" allowfullscreen></iframe>

* [Otra máquina de Turing hecha con Legos](http://rubens.ens-lyon.fr/)

Materia Prima
-------------

Clases:

* Scott Aaronson: Quantum Computing Since Democritus, 2006
  <http://www.scottaaronson.com/democritus/>.
  Hasta Lecture 8 (Crypto).

  Ideas:
      * MT para demostrar teorema de incompletitud de Gödel
      * reducción de Turing
      * Church-Turing Thesis
      * "Computing Machinery and Intelligence"/test de Turing
      * computadores compitiendo contra milles de milliones
        de años de evolución
      * Time Hierarchy Theorem
      * como presentar NP
      * Cook reducción, Karp reducción
      * 3SAT, CircuitSAT, Cook-Levin Theorem with circuits
      * Ladner Theorem con idea de la prueba
      * uso randomness
      * randomness and non-uniformity
      * historia de PRIMES
      * pseudorandom generators, one-way functions, crypto

* Scott Aaronson: Automata, Computability and Complexity, 2008-2013
  <http://stellar.mit.edu/S/course/6/sp13/6.045/>


  Ideas: <http://stellar.mit.edu/S/course/6/sp13/6.045/courseMaterial/topics/topic8/lectureNotes/6045-Lecture_7/6045-Lecture_7.pdf>
      * decidable (machine always halts) vs recognizable languages 
      * TM that solve problems for other domains
          * how to encode graphs
          * deterministic finita automatas
          * turing machines!
      * halting problem (con slides)
      * [ejemplos de lenguajes no decidibles](http://stellar.mit.edu/S/course/6/sp13/6.045/courseMaterial/topics/topic8/lectureNotes/6045-Lecture_8/6045-Lecture_8.pdf)
      * Post Correspondence Problem (Computation histories to prove it)
      * [Reducciones, Rice's Theorem y aplicaciones](http://stellar.mit.edu/S/course/6/sp13/6.045/courseMaterial/topics/topic8/lectureNotes/6045-Lecture_9/6045-Lecture_9.pdf)
      * [analisis lenguaje On1n en tiempo](http://stellar.mit.edu/S/course/6/sp13/6.045/courseMaterial/topics/topic8/lectureNotes/6045-Lecture_12/6045-Lecture_12.pdf)
      * pertinencia de Time Complexity Classes (1 tape vs multi tapes)
      * P, PATH, hierarchy theorem
      * Cook Levin with traditional proof
      * [ejemplos problemas NP hard](http://stellar.mit.edu/S/course/6/sp13/6.045/courseMaterial/topics/topic8/lectureNotes/6045-Lecture_16/6045-Lecture_16.pdf)

* <http://www.complexity.ethz.ch/education/Lectures/ComplexityHS10/Script>
      * Mahaney's Theorem (proof)
      * Ladner's Theorem (proof)
      * Circuits and TM

* [Nabil Mustafa](http://sma.epfl.ch/~moustafa/Other/Complexityslides/)
      * lower bound time for PAL (1)
      * 2SAT in P (3), 2-Coloring
