% Programación Declarativa: Libería Brick

![](brick.svg)

[Brick](https://hackage.haskell.org/package/brick) es una librería Haskell que permite
hacer interfaces de usuario en modo texto.


# Instalación

~~~bash
cabal install brick
~~~

o

~~~bash
stack install brick
~~~

# Interfaz sin estado

##  Ejemplo  `ReadmeDemo.hs` : una interfaz no-interactiva

~~~haskell
module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

ui :: Widget ()
ui =
    withBorderStyle unicode $
    borderWithLabel (str "Hello!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))

main :: IO ()
main = simpleMain ui
~~~

Compilar y ejecutar con:

~~~bash
ghc ReadmeDemo.hs -package brick  -threaded
./ReadmeDemo
~~~

Explicación (es útil buscar en [Hoogle](https://hoogle.haskell.org/) también):

* `simpleMain`: una función main simple que toma un widget y lo renderiza. El bucle de evento termina cuando el usuario
  aprieta una tecla, pero los cambios de tamaño de la terminal provocan que se dibuje de vuelta la interfaz.
* `<+>` toma dos widgets y los une horizontalmente
* `center` toma un widget y lo centra vertical y horizontalmente dentro de su espacio disponible.
* `str` toma una String y crea un widget donde se muestra la string
* `vBorder` es una línea vertical que llena todo el espacio disponible
* `borderWithLabel` toma dibuja un rectángulo alrededor de un widget (su 2ndo parámetro) y además este rectángulo tiene una
  etiqueta que es un widget (su 1e parámetro)
* `withBorderStyle` modifica el estilo de un widget, `unicode` es un estilo que usa caracteres unicode para que los ángulos
  parezcan "perfectos", si ponés `ascii` se ve menos prolijo.

Otras funciones y operadores que podés probar en este programa:

* `<=>` toma dos widgets y los une verticalmente (los apila)
* `hBox` toma una lista de widgets y los une horizontalmente
* `vBox` idem pero verticalmente
* `hBorder` es una línea horizontal que llena todo el espacio disponible

# Interfaz para una aplicación con estado

## El tipo `App` y `defaultMain`

El tipo `App` es un tipo registro que provee varias funciones:

~~~haskell
data App s e n =
    App { appDraw         :: s -> [Widget n]
        , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
        , appHandleEvent  :: s -> BrickEvent n e -> EventM n (Next s)
        , appStartEvent   :: s -> EventM n s
        , appAttrMap      :: s -> AttrMap
        }
~~~

El tipo `App` está parametrizado sobre tres tipos. Estas variables de tipo aparecen
en los protótipos de funciones y tipos de la librería `brick`. Son:

* el tipo del estado de la aplicación `s`:
  el tipo de dato que va a evolver durante la ejecución de la aplicación. Tu
  aplicación provee a la librería con su valor inicial y el "event handling"
  lo transformará mientras se ejecuta el programa. Cuando una aplicación `brick`
  termina, se devuelve el valor final del estado.
* el tipo evento `e`:
  el tipo de eventos que la aplicación deberá producir y gestionar en `appHandleEvent`.
  Todas las aplicaciones reciben eventos desde la librería subyacente `vty`, tales como
  eventos del teclado o cambio de tamaño de la ventana. La variable de tipo indica
  el tipo de eventos *adicionales* que la aplicación necesitará.
* el tipo de nombre de recurso `n`:
  durante la ejecución de la aplicación, a veces se necesita referirse al estado del
  renderizador, tal que el espacio ocupado por algún widget, el estado de una ventana
  desfilable, el click del mouse, o la posición de un cursor. Para estas situaciones,
  creamos un gestor único llamado *nombre de recurso* (ressource name). El tipo `n`
  especifica el tipo de nombre que la aplicación usa para identificar estos pedazos de
  estado producidos y manejados por el renderizador.

## Ejecutar una aplicación

Para ejecutar una `App`, la pasamos a `Brick.Main.defaultMain` o `Brick.Main.customMain`
junta con el valor inicial del estado de la aplicación:

~~~haskell
main :: IO ()
main = do
  let app = App { ... }
      initialState = ...
  finalState <- defaultMain app initialState
  -- Use finalState and exit
~~~

La función `customMain` es para usos más avanzados.


## `appDraw`: dibujar una interfaz

El valor de ``appDraw`` es una función que convierte el estado actual
de la aplicación en una lista de *capas* de tipo `Widget`, listadas desde
la más arriba, que constituyen la intefaz. Cada `Widget` se convierte en una
capa `vty` y las capas resultantes se dibujan en la terminal.

El tipo `Widget` es el tipo de las *instrucciones de dibujo*, y usualmente
un `Widget` se construye combinando varios `Widget`. Estas instrucciones
se ejecutan tomando en cuenta 3 cosas:


- El tamaño de la terminal: esto determina cuántos valores de `Widget`
  se comportan. Por ejemplo, los `Widget` de tamaño fijo como las
  cadenas de caracteres, se comportan igual en todas las condiciones
  (y son recortadas si la terminal es demasiado chica) pero combinadores
  como `vBox` o `center` usan el tamaño de la terminal para determinar
  como disponer los otros widgets.
- El diccionario de atributos de la aplicación (`appAttrMap`):
  funciones de dibujo que usan atributos hacen que se lea el diccionario
  de atributos.
- El estado de cualquier ventana desfilable (scrollable viewport) de los
  dibujos *previos* será considerado.

La función `appDraw` es llamada cuando el bucle de evento empieza a dibujar
la aplicación cuando aparece por primera vez. También es llamada justo después
de que se procesa un evento por `appHandleEvent`. 

Las funciones de dibujo se encuentran en `Brick.Widgets.Core`, y existen
otros módulos `Brick.Widgets` para usos más específicos.


## `appHandleEvent`: manejar eventos

`appHandleEvent` es una función que decide como modificar el estado
de la aplicación como resultado de un evento:

~~~haskell
appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
~~~

El parámetro de tipo `s` es el estado de la aplicación cuando ocurre
el evento. `appHandleEvent` decide cómo cambiar el estado según el evento
y luego lo devuelve.

El segundo parámetro de tipo `BrickEvent n e` es el evento en sí.
Las variables de tipo `n` y `e` corresponden al *tipo del nombre de recurso*
y *tipo de eventos* de su aplicación, respectivamente, y deben corresponderse
con los tipos en `App` y `EventM`.

El tipo del valor de devolución  `Next s` describe lo que debería pasar
kuego de que el gestor de eventos termina. Tenemos tres opciones:

* `Brick.Main.continue s`: continúa ejecutando el bucle de eventos con el
  estado `s` como siguiente valor. Usualmente es donde modificarías el
  estado según el evento y lo devolverías.
* `Brick.Main.halt s`: interrumpe el bucle de eventos y devuelve el valor
  final del estado de la aplicación `s`. Este valor es devuelvo al llamador
  de `defaultMain` o `customMain` donde puede ser utilizado antes de salir
  del `main`.
* `Brick.Main.suspendAndResume act`: suspende el bucle de eventos de `brick`
  y ejecuta la acción `IO` especificada `act`. La acción `act`
  debe ser de tipo `IO s`, entonces cuando se ejecuta debe devolver el siguiente
  estado de la aplicación.
  Cuando se usa `suspendAndResume`, el bucle de eventos de `brick`
  se apaga y el estado de la terminal es restaurado a su estado de cuando
  el bucle empezó su ejecución. Cuando termina ese momento, el bucle de eventos
  retoma su ejecución utilizando el valor de estado devuelto.
  Es útil para situaciones donde el programa necesita suspender su interfaz
  y ejecutar otro programa que necesita controlar la terminal
  (por ejemplo un editor de texto).

La mónada ``EventM`` sirve para manejar los eventos. Esta mónada es un transformador
alrededor de `IO` entonces podés hacer acciones I/O en esta mónada usando la función
`liftIO`.  Tomá en cuenta que el tiempo pasado en tu gestor de eventos es tiempo durante
el cual la interfaz de usuario no responde, entoces tomalo en cuenta cuando decidís
si vas a tener hilos para hacer algún trabajo en lugar de hacer el trabajo en el gestor
de eventos.

## Arrancar: `appStartEvent`

Cuando empieza una aplicación, puede ser útil hacer algunas acciones típicamente
solo posibles cuando un evento ocurrió, por ejemplo inicializar algún estado de
ventana desfilable. Dado que acciones como esta solo se pueden llevar a cabo en un
`EventM` y dado que no queremos esperar hasta que ocurra el primer evento para
hacer ese trabajo en `appHandleEvent`, el tipo `App` provee la función `appStartEvent`
con este propósito:

~~~haskell
appStartEvent :: s -> EventM n s
~~~

Esta función toma el estado inicial y lo devuelve dentro de un `EventM`, posiblemente
haciendo pedidos de ventanas desfilables. Esta función es llamada una y solamente una
vez, cuando empieza la aplicación. En general, querés simplemente definirla como `return`
para la mayoría de las aplicaciones.


## Ubicar el cursor: `appChooseCursor`

El proceso de renderización de un `Widget` puede devolver información sobre donde
ese widget quiere ubicar el cursor. Por ejemplo, un editor de texto necesita indicar
la posición del cursor. Sin embargo, dado que un `Widget` puede ser constituido de varios
widgets que ubican el cursor, debemos tener una manera de elegir cuál de las posiciones
de cursor reportadas, si hay, es la que queremos reconocer.

Para elegir cual ubicación de cursor usar, o no decidir de imprimir ninguno, definimos
la función:

~~~haskell
appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
~~~

El bucle de eventos renderiza la intefaz y colecta los valores de `Brick.Types.CursorLocation`
producidos por el proceso de renderización y pasa estos valores, juntos con el estado de la
aplicación, a esta función. Usando el estado de la aplicación (por ejemplo para saber qué
campo de formulario de texto está "enfocado") podés decidir cuál de las ubicaciones devolver,
o devolver `Nothing` si no querés mostrar un cursor.


# Documentación y ejemplos

## Más documentación

* En [esta página](https://github.com/jtdaugherty/brick/blob/master/docs/programs-screenshots.md) encontrarás capturas de pantalla de programas de demostración, junto con su código fuente.
* [Este tutorial](https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md) nos guia a través del código de un juego de serpiente.

Consulta [la guía oficial (en inglés) de Brick](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst).

# Algunas fuentes interesantes

* [Hoogle terminal GUI](https://github.com/andrevdm/bhoogle)
* [Minesweeper clone](https://dev.to/dkurilo/minesweeper-clone-in-haskell-for-console-321g)
* [Charla: "write a text editor in Haskell with Brick"](https://cs-syd.eu/posts/2020-02-27-talk-writing-a-text-editor-in-haskell-with-brick)
