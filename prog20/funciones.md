# 4. Funciones que toman parametros arreglos

![cantidad de participantes](group64.png){width=1cm} 1 o 2 participantes.


En un programa **arreglos.c** definir una constante para el tamaño
de todos nuestros arreglos:

~~~C
#define T 10
~~~

Definir las funciones siguientes:

* `void mostrar(const int a[])`: imprime el arreglo
* `void llenar_alea(int a[])`: llena el arreglo de valores aleatorios entre 0 y 99.
* `void invertir(const int a[], int b[])`: copia el contenido del arreglo `a`
en orden inverso en el arreglo `b`.

Usar esas funciones desde el `main`, como sigue:

~~~C
main(){
  int x[T];
  int y[T];
  llenar_alea(x);
  invertir(x,y);
  mostrar(x);
  mostrar(y);
}
~~~




# 5. Búsqueda en arreglo

![cantidad de participantes](group64.png){width=1cm} 1 o 2 participantes.

En un programa **busqueda.c**,
definir una constante ERROR con el valor -1.

Implementar la función
`int buscar(const int arr[], const int elem)`, que
realiza la búsqueda de un elemento en un arreglo.

Esta función devuelve un valor de tipo `int`, que
tiene que ser:

* el primer `i` tal que `elem == arr[i]`, si existe
* el valor `ERROR`, cuando el elemento buscado no
  está en el arreglo.

Unos ejemplos:

|  `arr`   |  `elem`  | `buscar(arr,elem)` |
|----------|----------|--------------------|
| [2,10,4] |  2       |  0                 |
| [2,10,4] |  4       |  2                 |
| [3,7,7]  |  7       |  1                 |
| [3,7,3]  |  3       |  0                 |
| [3,7,3]  |  4       |  `ERROR`           |
 
Probar esta función desde el main sobre dos arreglos
definidos manualmente.


