% MD3: Coloquio 2019

# Presentación general

Este proyecto se puede hace en grupos de 1 o 2 alumnos.

Fecha: antes del primer final, miércoles 5/12.

El objetivo es escribir un software - en el lenguaje que prefieran,
pero que pueda ejecutarse en línea de comando -
capaz de simular máquinas de Turing.

El proyecto debe ser entregado por mail el mismo día de la presentación a <guillaume.hoffmann@conicet.gov.ar>.

Se debe entregar el código fuente, una breve presentaciones e instrucciones para compilarlo.

El proyecto se divide en las tareas siguientes.

# 1. Poder ejecutar una Máquina de Turing una cantidad finita de pasos

Vamos a seguir el modelo de máquinas de Turing que vimos en la primera clase
del semestre.

Es decir, consideramos máquinas de Turing binarias (solo con símbolos 0 y 1),
de una sola cinta (infinita en las dos direcciones) inicializada a cero.

El cabezal solo puede moverse de un paso a la derecha o un paso a la
izquierda (no puede permanecer inmóvil).

La máquina no tiene estado de aceptación o de rechazo, pero tiene una
instrucción especial llamada `FIN`, que detiene la ejecución de la máquina.

Definir la representación de estas máquinas de Turing en su software.
Eso puede ser un tipo de datos dedicado, o alguno tipo de datos existente
especializado para tal fin.



# 2. Mostrar la tabla de alguna máquina (5 puntos)

Definir una función capaz de tomar una máquina de Turing e imprimirla
como tabla.

# 3. Ejecutar una máquina con cierta cantidad máxima de pasos (10 puntos)

Definir una función que haga eso. Tiene que tomar solamente una máquina y
una cantidad limite de pasos. Tiene que devolver si la máquina se detuvo antes
del limite de pasos (en ese caso, cuántos pasos hizo), o si la máquina todavía
seguía ejecutándose.

El software podrá contener algunas definiciones de máquinas de Turing para
probar esta función.

# 4. Enumerar todas las máquinas de cierto tamaño (10 puntos)

Definir una función que enumere todas las máquinas de cierto tamaño.
Por "tamaño", se entiende la cantidad de estados.

La función puede aceptar opciones para no enumerar máquinas equivalentes,
por ejemplo, no enumerar dos máquinas "en espejo". Hay más equivalencias
posibles.

En el caso de implementar este proyecto con un lenguaje peresozo como Haskell,
se puede devolver una lisa de todas las máquinas de cierto tamaño. En el caso
de usar un lenguaje más clásico, conviene hacer una función que toma una
máquina y devuelve la máquina siguiente en la enumeración.

# 5. Para cada cantidad de estados a partir de 2, enumerar todas las máquinas e imprimir cierta cantidad de máquinas interesantes (10 puntos)

Esta parte es la más libre. Está demostrado que no es posible encontrar,
por una cantidad dada de estados, la máquina que más tiempo se ejecuta antes de detenerse.

Pensar en como aproximarse a ese resultado. Puede pedir cierta interactividad al usuario. Por ejemplo,
pedir cierta cantidad de pasos al usuario y devolverle cuales máquinas son las más interesantes dentro
de este limite y cuales siguen ejecutandose. Proponer al usuario doblar esta cantidad de pasos y solo ejecutar
las máquinas que no terminaron, para ver si es el caso que terminen.

# 6. Enumeracion de máquinas no borrantes (10 puntos)

Agregar una opcion a este software para que solo haga esta enumeracion dentro de las máquinas no borrantes.

Son las máquinas que, una vez que escribieron un 1 en la cinta, no pueden volver a reemplazarlo por un 0.

(¿Por qué queremos buscar máquinas no borrantes interesantes? Porque son máquinas cuya ejecucion se puede
escribir en una hoja de papel sin nunca borrar lo que ya escribimos.)

# Competencia

Una vez que se entregan todos los proyectos, se podrá contestar a la pregunta siguiente:

¿Quién encontró la máquina de Turing no borrante de 5 estados con mayor cantidad
de pasos antes de detenerse (con vocabulario $\{1,0\}$ y cinta inicial puesta a cero)?

Misma pregunta para 10, 15, etc.

# Plagio

Una condición imperativa es que el software sea de la autoría exclusiva de
los alumnos que lo entregan. Si el software tiene partes escritas por terceros,
eso debe estar especificado en el código fuente. Todo caso de plagio recibirá
directamente 0 punto.

# Notas posibles

* 3: 
* 4
