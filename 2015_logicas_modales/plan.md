FACULTAD DE FILOSOFÍA, HUMANIDADES Y ARTES

DEPARTAMENTO DE POSGRADO

PLANIFICACIÓN DE CÁTEDRA

CURSO DE POSGRADO:  LÓGICAS MODALES COMPUTACIONALES

DOCENTE: DR. GUILLAUME HOFFMANN

AÑO 2015

CURSO DE POSGRADO:

LÓGICAS MODALES

Introducción

	El curso de posgrado  “Lógicas Modales Computacionales” está
enfocado con el objetivo de familiarizar al investigador con los
resultados y técnicas esenciales relativos a las mismas, la cual tiene
conexiones con diversos campos de investigación como la Lógica
Proposicional, la Lógica Clásica (o Lógica del Primer Orden), y la
Complejidad Computacional.

Se pretende en este curso dar una visión global de los temas esenciales
de esta área y que sean de interés, en particular, para matemáticos y
especialistas en Ciencias de la computación. Entre estos tópicos se
encuentran:  sintáxis y semántica de la Lógica Modal Básica,
Traducción Estandard a Primer Orden, bisimulaciones, caracterización
de van Benthem, propiedad de modelos árbol y modelos finitos,
decidibilidad y complejidad del problema de Satisfacibilidad, algoritmos
de tableaux y de traducción a Teorías de satisfacibilidad módulo
(SMT).

Indicamos finalmente que el presente curso tiene un carácter
autocontenido que permitiría al potencial alumno, dar cuenta de los
sucesivos contenidos a impartirse. De cualquier modo es recomendable que
el interesado posea conocimientos mínimos en Lógica Matemática y/o
Computación.

 Marco teórico

	En su perspective más moderna, las lógicas modales son vistas como
herramientas para indentificar fragmentos interesantes de lógicas de
primer y alto orden, para una determinada tarea de modelaje o
inferencia. “Interesante” usualmente significa: de baja complejidad,
buenas propiedades metalógicas (completitud, interpolación, etc.),
poder expresivo adecuado, simple diseño de algoritmos de decisión y de
herramientas automáticas de inferencia, simplicidad de uso, etc.

Por el otro lado, las posibles ``tareas de modelaje o inferencia''
pueden ser extremandamente variadas, desde aplicaciones clásicas en
filosofía y epistemología, usos en linguística, a los usos más
proximos a la Ciencia de la Computación como representación del
conocimiento, model checking y verificación formal de software y
hardware.

El principal énfasis teórico del curso será en los aspectos
computacionales de lenguages modales (incluyendo lenguajes modales no
standard, como lógicas híbridas, con memoria, dinámicas y lógicas
para descripción). En particular, discutiremos diferentes métodos de
inferencia y decisión (directos e indirectos), su complejidad y
posibles heurísticas y optimizaciones para problemas de
satisfiabilidad.

El curso tiene un enfoque semántico, es decir que veremos las lógicas
como lengajes que describen modelos. En cuanto a los algoritmos de
deducción, estudiaremos algoritmos que construyen modelos.
Presentaremos una selección de herramientas para deducción automática
(HTab, Z3, veriT), complementando las nociones teóricas con experiencia
de laboratorio.

El curso no requiere conocimientos previos de lógica modal, pero sí
asume conocimientos básicos sobre lógica proposicional y de
predicados.

Objetivos generales

	Se espera que quienes aprueben este curso sean capaces de:

Desarrollar un modo de razonamiento, intuición y actitud críticos, que
les permita hacer frente a diferentes desafíos dentro de su experiencia
en la investigación matemática.

Manifestar una actitud positiva hacia el trabajo, en líneas generales.

Adquirir el hábito de plantear y resolver situaciones nuevas,
recurriendo a su capacidad creadora.

Aplicar los conocimientos adquiridos de forma creativa, con miras a un
desarrollo satisfactorio en sus capacidades de investigación.

Participar activamente de las clases teóricas y prácticas.

Dominar las distintas fuentes bibliográficas, estableciendo vínculos
entre diferentes libros, artículos o monografías, sean en castellano o
en otro idioma accesible al alumno.

Unidades de aprendizaje

	Las unidades de aprendizaje previstas en el curso son 5 (cinco) en
total. Deseamos indicar que durante el tiempo de dictado previsto para
este curso es totalmente viable el dictado los contenidos que se
muestran a continuación. De este modo, el tiempo impartido en el
dictado de cada una de las siguiente unidades permitirá el desarrollo
del curso respetando el cronograma que se sugiere más adelante.

UNIDAD 1 . Lógicas Modales como Fragmentos de Lógicas Clásicas.

 Objetivos Específicos:

Ubicar la Lógica Modal Básica (LMB) con respecto a la Lógica de
Primer Orden y a la Lógica Proposicional.

Conocer otros operadores modales que pueden extender la LMB.

Contenidos: Repaso de Lógica de Primer Orden. Lógica Modal Básica.
Sintaxis y Semántica.  Motivación. Otros Operadores Modales. La
Traducción Standard. Transferencia de Resultados.

UNIDAD 2:  Teoría de Modelos I

Objetivos Específicos: 

Entender las bisimulaciones.

Saber demostrar la diferencia de expresividad entre dos lenguajes
modales.

Contenidos: Isomorfismos Potenciales. Bisimulaciones. Bisimulación y
Poder expresivo. Clases Hennessy-Milner. Clausura de modelos.
Caracterización de van Benthem. Definibilidad.

UNIDAD 3:  Teoría de Modelos II

Objetivos Específicos:

Saber demostrar la propiedad de modelo árbol.

Saber demostrar la propiedad de modelos finitos y su consecuencia en
decibilidad del problema de satisfacibilidad.

Contenidos: Propiedad de Modelos Arbol. Propiedad de Modelos Finitos.
Decidibilidad. Filtraciones.

UNIDAD 4: Complejidad.

Objetivos Específicos:

Saber demostrar resultados de complejidad de distintas Lógicas Modales
usando reducciones.

Contenidos: Repaso de clases de complejidad. Logicas Modales en NP.
Lógicas Modales en PSpace. Lógicas Modales en ExpTime. Lógicas
Modales indecidibles. Model Checking. 

UNIDAD 5: Algoritmos para SAT Modal

Objetivos Específicos: 

Saber demostrar la adecuación del método de tableaux para lógica
modal básica.

Conocer extensiones de algoritmos de tableaux para otros operadores
modales.

Saber usar un demostador automático (solver).

Contenidos: Método de Tableaux. Correctitud, Completitud. Complejidad.
Implementaciones. Traducción a SMT.

5) Metodologías de Enseñanza – Aprendizaje

	Por tratarse de un curso de posgrado, queda claro que el profesor que
imparte el curso dará a los asistentes al mismo los lineamientos
generales de los temas a tratarse, y los guiará en el estudio de dichos
temas, de un modo mucho mas libre que en un curso de grado. 

	Específicamente, se pretende para cada una de las partes involucradas
lo siguiente:

A) EL PROFESOR:

- Presentará y explicará el esquema de cada unidad.

- Enunciará los objetivos operacionales de la misma.

- Proporcionará información básica y guiará al alumno en la
búsqueda de nuevas fuentes de información.

B) EL ASISTENTE AL CURSO:

- Estudiará los apuntes, artículos o textos sugeridos por el docente,
en forma indivudual o grupal.

- Resolverá los ejercicios propuestos por el docente, e intentará
plantear situaciones problemáticas  novedosas, generalizando la
actividad realizada.

 - Realizará estudios de forma independiente, con apoyo del material
bibliográfico sugerido por el docente o por cualquier material que el
propio alumnno juzgue adecuado.

 - Realizará trabajos escritos y será capaz de exponerlos oralmente,
de acuerdo a lo requerido por el docente.

5) Régimen de Aprobación

	Para el presente curso de posgrado se ha optado por seguir un régimen
de evaluación continua, del siguiente modo:

En cada clase el docente realizará una puesta a punto, en la que
evaluará sintéticamente y oralmente los conocimientos que se
impartieron en la clase anterior.

Al finalizar la parte expositiva del curso, se realizará una puesta a
punto oral y grupal entre los asistentes al curso y el docente.

Después de finalizado el curso, y con un plazo razonable, los
asistentes al mismo deberán presentar una monografía con temas
sugeridos por el docente, en donde se prevé un aporte original por
parte de los asistentes. Debe indicarse que esta última parte del curso
es esencial. De hecho, toda la estructuración previa a esta instancia
está, principalmente, orientada a la redacción del trabajo final.

6) Cronograma

	Dado que se trata de una materia de posgrado, la misma presupone una
mayor creatividad del alumno, lo que implica una carga horaria
presencial menor que en una carrera de grado. Obviamente, esta libertad
de acción debera ser compensada por una mayor dedicación del alumno en
horas de estudio individuales o grupales, o en horas de consulta.

	En el caso específico de este curso, al ser impartido por un docente
de otra universidad, están previstas cuatro visitas (a razón de una
visita por mes), cantidad mínima para impartir los contenidos del
curso, constituido por ocho unidades breves. Además está prevista una
visita extra, a los fines de una puesta a punto final, en una reunión
en la que se comentará y se evaluará el trabajo final que presentarán
los alumnos.

	Durante cada visita el docente realizará un dictado intensivo del
curso. Por tal motivo, cada visita, de dos días de duración, se
subdividirá en cuatro módulos de aproximadamente dos horas y media
(dictándose dichos módulos en horarios de mañana y de tarde). De esto
se contabiliza que la parte presencial del curso constará de 40 horas
(considerando solamente cuatro clases). A esto debe agregarse la
duración no presencial del mismo, teniendo en cuenta la redacción de
la monografía arriba indicada, que es requisito esencial para la
aprobación del curso. Por tal motivo, se considera que el curso tiene
una duración estimada de 60 horas.

	La distribución horaria de la parte presencial del curso es
sintetizada en el siguiente esquema, que además incluye la
distribución tentativa de los contenidos del curso.

VISITA 1: 

Módulo 1: Unidad I.

Módulo 2: Unidad I.

Módulo 3: Unidad I (final). Puesta a punto

Módulo 4: Unidad II (introducción). 

VISITA 2: 

Módulo 1: Repaso de la visita anterior. Unidad II (continuación).

Módulo 2: Unidad II (final).

Módulo 3: Unidad III.

Módulo 4: Unidad III (continuación).

VISITA 3: 

Módulo 1: Repaso de la visita anterior. Unidad III (final).

Módulo 2: Puesta a punto de Unidades I a III.

Módulo 3: Unidad IV.

Módulo 4: Unidad IV (continuación).

VISITA 4: 

Módulo 1: Repaso de la visita anterior. Unidad IV (final).

Módulo 2: Unidad V (introducción).

Módulo 3: Unidad V (final).

Módulo 4: Puesta a punto general, y lineamientos generales para la
monografía. 

VISITA 5: 

Ésta visita está comprendida en dos módulos, considerando que se
trata de la puesta a punto y la evaluación final:

Módulo 1: Reunión individual con cada uno de los asistentes, para
revisión de la monografía.

Módulo 2: Reunión grupal, con exposición de las monografías, y
evaluación de las mismas.

	Tentativamente, se tiene previsto que la primera clase se realice en el
día 27 de Julio de 2014, en horario de mañana (9:00 hs). De este modo,
las subsiguientes visitas están previstas para Agosto, Septiembre,
Octubre y Diciembre de 2013. 

	

7) Aranceles

	Está previsto que el arancel del presente curso sea de … pesos, que
es el valor estipulado en el Depto.de Posgrado de la FFHA para cursos de
hasta … horas de duración. Por otro lado, de acuerdo a lo sugerido
por el Departamento de Matemática (el cual avala el presente curso de
posgrado), se sugiere que los docentes del mismo, así como los
investigadores del Instituto de Ciencias Básicas, sean beneficiados con
una beca, consistente en la exención del pago del porcentaje referido
al salario del docente. De este modo, los beneficiarios solamente
pagarán el importe reservado al uso del Departamento de Posgrado de la
FFHA (30%).

8) Destinatarios

	Por tratarse de un curso de posgrado, los asistentes al mismo deberán
poseer título universitario. Además, se indica que este curso puede
resultar de particular interés para los profesionales que tengan
vinculación con la Lógica Fornal y/o con la Teoría de la
Computación. En particular, Matemáticos, Informáticos, Ingenieros en
Sistemas, Lingüistas Formales, Filósofos de la Lógica, etc.).

APENDICE: PROGRAMA ANALÍTICO Y DE EXAMEN

Universidad Nacional de San Juan

Facultad de Filosofía, Humanidades y Artes

Departamento de Posgrado

Perfil del alumno: alumnos que estén realizando posgrados en
Matemática o disciplinas afines a la Lógica (Informática, Filosofía,
Lingüística Fornal, Ingeniería en Sistemas).

Materia: Lógicas Modales Computacionales

Docente: Dr. Guillaume Hoffmann

PROGRAMA SINTÉTICO

UNIDAD 1: Lógicas Modales como Fragmentos de Lógicas Clásicas.

UNIDAD 2: Teoría de Modelos I

UNIDAD 3: Teoría de Modelos II

UNIDAD 4: Complejidad.

UNIDAD 5: Algoritmos para SAT Modal.

PROGRAMA ANALÍTICO Y DE EXAMEN

UNIDAD 1: Lógicas Modales como Fragmentos de Lógicas Clásicas.

Contenidos: Repaso de Lógica de Primer Orden. Lógica Modal Básica.
Sintaxis y Semántica.  Motivación. Otros Operadores Modales. La
Traducción Standard. Transferencia de Resultados.

UNIDAD 2: Teoría de Modelos I.

Contenidos: Isomorfismos Potenciales. Bisimulaciones. Bisimulación y
Poder expresivo. Clases Hennessy-Milner. Clausura de modelos.
Caracterización de van Benthem. Definibilidad.

UNIDAD 3:  Teoría de Modelos II.

Contenidos: Propiedad de Modelos Arbol. Propiedad de Modelos Finitos.
Decidibilidad. Filtraciones.

UNIDAD 4: Complejidad.

Contenidos: Repaso de clases de complejidad. Logicas Modales en NP.
Lógicas Modales en PSpace. Lógicas Modales en ExpTime. Lógicas
Modales indecidibles. Model Checking. 

UNIDAD 5:  Algoritmos para SAT Modal

Contenidos:  Método de Tablaux. Correctitud, Completitud. Complejidad.
Implementaciones. Traducción a Primer Order. Traducción a SMT.

BIBLIOGRAFÍA

“Modal Logics: a semantic perspective”. Patrick Blackburn and Johan
van Benthem. Handbook of Modal Logics. Elsevier. 2006.

“Modal Logic”. Patrick Blackburn, Maarten de Rijke and Yde Venema.
Cambridge University Press, 2002.

“First Steps in Modal Logic”. Sally Popkorn. Cambridge University
Press. 1994.

“The ultraproduct construction”. Jerome Keisler. In "Ultrafilters
Across Mathematics", ed. by V. Bergelson et. al., Contemporary
Mathematics 530 (2010), pp. 163-179, Amer. Math. Soc. 

“Modal Satisfiability via SMT Solving”. Carlos Areces, Pascal
Fontaine, and Stephan Merz. Software, Services, and Systems: Festschrift
Martin Wirsing. 

© Springer, LNCS 8950, pp. 30-45 (2015).

