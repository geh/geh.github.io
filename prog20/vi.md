% Manual de `busybox vi`

`vi` es un programa que permite editar archivos de texto.
Es muy liviano, solo requiere un terminal de texto para ser usado.
En dispositivos embedidos, suele haber una versión proveida
por el conjunto de herramientas [BusyBox](https://es.wikipedia.org/wiki/Busybox).
BusyBox es también instalado en el sistema JSLinux.

En esta página vamos a ver buena parte de las funcionalidades de `busybox vi`.
Al final de la página se encuentra un resumen de comandos.

# Usar `vi` #

## Arrancar `vi` #

Se arranca el editor `vi` desde la línea de comando dándole el nombre del archivo
que queremos editar con `vi archivo`. Si el archivo `archivo` existe en la carpeta
actual, el editor cargará su contenido. Si no existe, arrancará sin contenido,
y el archivo se creará cuando guardemos el contenido del editor.

Si se arranca `vi` sin dar un nombre de archivo, habrá que indicarlo cuando queramos
guardar lo que habremos escrito.

## Modos de operación ##

`vi` es un editor modal, es decir, las teclas que tipeamos se interpretan
de maneras distintas según el modo en qué estamos.
Es una característica muy poderosa pero también requiere cierto aprendizaje.

`busybox vi` tiene tres modos de operación:

* modo comando
* modo inserción
* modo reemplazo (el menos usado)

El `modo comando` es el modo por defecto cuando arrancamos `vi`.
Se puede siempre volver a ese modo usando la tecla `ESC`.
Los modos `inserción` y `reemplazo` son para editar texto. La única diferencia es que en modo
reemplazo, los carácteres bajo el cursor son reemplazados, mientras que en `modo inserción`
son desplazados a la derecha.

En el modo comando, cuando tipeamos `comandos dos-puntos`, es decir comandos que
empiezan con la tecla `:`, debemos validarlos con la tecla `ENTER`.

El modo actual está indicado por un carácter en el rincón inferior izquierdo de la pantalla:

* `-` significa `modo comando`,
* `I` significa `modo inserción`,
* `R` significa `modo reemplazo`.

## Mover el cursor ##

Cuando arrancamos vi, estamos en `modo comando`.
Eso significa que no podemos ingresar texto, pero (entre otras cosas) podemos
mover el cursor. La manera más simple de moverse en `vi` es usando
las teclas flechas.

## Edición básica ##

Para tipear algun texto, necesitamos pasar al modo inserción.
Lo podemos hacer con la tecla `i` (para insertar antes de la posición actual del cursor).
Después de insertar texto, usamos `ESC` para volver al modo comando. Cuando estamos en modo
inserción, podemos también suprimir texto usando las teclas `SUPRIMIR` o `RETROCESO`.

## ¿Cómo guardar el archivo y salir del editor? ##

Si estamos editando un archivo que ya tiene nombre, podemos usar `ZZ`
en modo comando. Significa "guardar y salir" (e irse a dormir, por eso "ZZ...").

Para guardar los cambios sin salir, podemos usar el comando `:w`
(apretamos la tecla dos-puntos y luego tipeamos `w`, luego apretamos `ENTER`).
En el caso que todavía `vi` no sepa el nombre
del archivo (porque lo hemos arrancado sin especificarlo) debemos agregarlo
al comando de la forma siguiente: `:w archivo.c`. 

Para salir, podemos usar el comando `:q`, pero `vi` no nos deja
salir así si hay modificaciones que no fueron guardadas al archivo.
Si no queremos guardar esos cambios y salir igual,
agregamos `!` al final del comando para forzar la salida: `:q!`.
Para guardar y salir en un solo comando podemos usar `:wq`.

En estos comandos, `w` significa "write" y `q` significa "quit".

## Copiar, borrar y pegar ##

El modo comando no es solo para mover el cursor. Hay unos comandos que
sirven para editar texto. El más básico es el comando suprimir. Apretamos
la tecla `x` para suprimir el carácter que está bajo el cursor, y con `X`
(mayúscula) suprimimos el carácter que está justo antes del cursor.

Podemos copiar una línea entera usando `yy` (o `Y`), y podemos borrarla
completamente usando `dd`. Una línea copiada o borrada es copiada
a una memoria temporaria, así que la podemos insertar después de la línea
actual usando `p`, o antes de la línea actual usando `P`.

## Más formas de moverse dentro de un archivo ##

Cuando editamos un archivo, solemos movernos bastante por todos lados.
`vi` tiene unos comandos útiles para hacerlo de manera más eficiente.

El comando `0` (o la tecla `HOME`) nos trae al principio de la línea,
mientras `$` (o la tecla `FIN`) nos lleva al final. En lugar de ir al principio
de la línea, si tenemos ganas de ir al *primer carácter no blanco* de la línea
actual, usamos la tecla `^`.

Podemos buscar secuencias de carácteres en el archivo.
Para hacerlo, usamos el comando `/xyz` (seguido de `ENTER`) para buscar hacia
adelante. Para repetir la búsqueda, usamos `n` (buscar la siguiente ocurrencia)
o `N` (la anterior).

Para saltar directamente a una línea cuya posición conocemo, por ejemplo 45,
podemos usar el comando `:45`.

Finalmente tenemos un comando muy útil, `%`, que saltar al símbolo que cierra
(o abre) el paréntesis, la llave o el corchete al que tenemos bajo el cursor.

## El comando punto ##

El comando punto (apretando la tecla `.`) repite el último comando que modificó el texto.
Por ejemplo, si acabamos de borrar una línea con `dd`, podemos borrar más
líneas apretando la tecla punto varías veces.

## Deshacer ##

`busybox vi` tiene un comando de "undo" simple con `u` (sin los dos-puntos).
Podemos cancelar varios cambios volviendo a apretar `u`.

Lamentablemente no existe el "redo" o "volver a hacer". Entonces ¡con cuidado!

# Resumen de comandos #

## Moverse ##

<table>
<tr><td>`h`</td><td>ir un carácter a la izquierda</td></tr>
<tr><td>`j`</td><td>ir una línea abajo</td></tr>
<tr><td>`k`</td><td>ir una línea arriba</td></tr>
<tr><td>`l`</td><td>ir un carácter a la derecha</td></tr>
<tr><td>`0`</td><td>ir al principio de la línea actual</td></tr>
<tr><td>`$`</td><td>ir al final de la línea actual</td></tr>
<tr><td>`w`</td><td>avanzar al principio de la palabra siguiente</td></tr>
<tr><td>`e`</td><td>avanzar al final de la palabra siguiente</td></tr>
<tr><td>`b`</td><td>ir atrás de una palabra</td></tr>
<tr><td>`%`</td><td>ir al símbolo correspondiente: () [] {}</td></tr>
<tr><td>`G`</td><td>ir a la última línea</td></tr>
<tr><td>`gg`</td><td>ir a la primera línea</td></tr>
</table>

Las teclas flecha, principio y fin también funcionan como esperado.

## Buscar ##

<table>
<tr><td>`/xyz`</td><td>buscar la secuencia de carácteres `xyz` hacia adelante</td></tr>
<tr><td>`n`</td><td>repetir la última búsqueda</td></tr>
<tr><td>`N`</td><td>repetir la última búsqueda hacía atrás</td></tr>
</table>

## Insertar texto ##

<table>
<tr><td>`i`</td><td>insertar antes del carácter actual</td></tr>
<tr><td>`a`</td><td>insertar después del carácter actual</td></tr>
<tr><td>`o`</td><td>insertar una línea después de la línea actual</td></tr>
<tr><td>`O`</td><td>insertar una línea antes de la línea actual</td></tr>
<tr><td>`I`</td><td>insertar antes del primer carácter no blanco de la línea actual</td></tr>
<tr><td>`A`</td><td>insertar despues del último carácter no blanco de la línea actual</td></tr>
</table>

## Suprimir texto ##

<table>
<tr><td>`x`</td><td>borrar el carácter bajo el cursor</td></tr>
<tr><td>`dd`</td><td>borrar la línea actual</td></tr>
<tr><td>`dM`</td><td>borrar hasta donde el movimiento M llevaría el cursor</td></tr>
<tr><td>`D`</td><td>borrar desde la posición actual hasta el fin de la línea</td></tr>
</table>

El texto borrado es puesto dentro del búfer temporario.

## Copiar y pegar texto con el búfer temporario ##

<table>
<tr><td>`yy`</td><td>copiar la línea actual en el búfer</td></tr>
<tr><td>`yM`</td><td>copiar texto al búfer hasta donde el movimiento M llevaría el cursor</td></tr>
<tr><td>`p`</td><td>pegar el texto del búfer después de la línea actual</td></tr>
<tr><td>`P`</td><td>pegar el texto del búfer antes de la línea actual</td></tr>
</table>

## Modificar texto ##

<table>
<tr><td>`rc`</td><td>reemplazar el carácter actual con `c`.</td></tr>
<tr><td>`cc`</td><td>reemplazar la línea actual por una vacía, entra al modo inserción</td></tr>
<tr><td>`C`</td><td>reemplazar desde la posición actual hasta el final de la línea</td></tr>
<tr><td>`R`</td><td>entrar al modo reemplazo</td></tr>
</table>

## Comandos empezando por dos-puntos ##
  
<table>
<tr><td>`:q`</td><td>salir</td></tr>
<tr><td>`:q!`</td><td>salir sin chequear que el archivo fue guardado</td></tr>
<tr><td>`:w`</td><td>guardar el archivo</td></tr>
<tr><td>`:wq`</td><td>guardar y salir</td></tr>
<tr><td>`:10`</td><td>ir a la línea 10</td></tr>
</table>

Estos comandos tienen que ser confirmados con la tecla `ENTER`.

## Otros ##

<table>
<tr><td>`u`</td><td>cancelar el último cambio</td></tr>
<tr><td>`.`</td><td>repetir el último comando de cambio</td></tr>
<tr><td>`ZZ`</td><td>guardar y salir</td></tr>
</table>

# Fuentes #

El contenido de esta página fue traducido y modificado a partir de las páginas siguientes:

* <http://www.phcomp.co.uk/Tutorials/Unix-And-Linux/Vi-and-vim-reference-sheet.html>
* <http://tinyurl.com/Adamski-vi>

