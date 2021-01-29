% Programación 1: parcial 1
% 2020-05-11

La nota total de este parcial - sobre 10 puntos - ya contiene
3 puntos de participación y entrega de las tareas semanales.

Criterios de regularidad:

* tener por lo menos 1 punto en la parte "pruebas de escritorio"
* tener por lo menos 2 puntos en la parte "programas"

# Programas (4 puntos)

Los ejercicios se deben hacer dentro de [JSLinux](http://tinyurl.com/prog1linux),
y con el editor de texto `vi`.

Cada programa deberá llamarse como indicado en el enunciado.

Para descargarlos, usar dentro de jslinux el comando `export_file`.

Luego mandarlos a <guillaumh@gmail.com> junto con una captura de pantalla del código dentro de JSLinux.

## 2.1 Estructuras condicionales

Escribí un programa **ordenar.c** que reciba como entrada tres números, y los muestre ordenados de menor a mayor:

~~~
Ingrese numero: 10
Ingrese numero: 1
Ingrese numero: 4
1 4 10
~~~

Evitá redundancias en las estructuras condicionales y usa el `else` cuando corresponde.

## 2.2 Bucles

Escribí un programa **suma.c** que pida al usuario dos números enteros, y luego entregue la suma de todos los números que están entre ellos. Por ejemplo, si los números son 1 y 7, debe entregar como resultado 2 + 3 + 4 + 5 + 6 = 20.

~~~
Ingrese num: 1
Ingrese num: 7
La suma es 20
~~~

Si el primer número es mayor al segundo, se debe dar el mismo resultado (intervertir los valores en ese caso):

~~~
Ingrese num: 7
Ingrese num: 1
La suma es 20
~~~

## 2.3 Cálculo de promedio

En un programa **promedio.c** declarar las variables siguientes:

```
int n;
int x;
float suma;
```

Hacer que este programa solicite valores enteros positivos al usuario, y los vaya sumando a la
variable `suma`.  Ni bien el usuario ingresa un valor negativo, se termina la carga y
no se toma en cuenta ese valor negativo.

Luego, se calcula y se imprime el promedio (usar la variable `n` para almacenar la cantidad de enteros ingresados).
Si no se ingresó ningun valor (positivo), imprimir un mensaje diciendo que no se puede calcular el promedio.

Finalmente, imprime ese promedio, con 2 decimales de precisión (en printf, usar el código %.2f en lugar de %f).

Comprobar con los valores siguientes: Ingresar 1, 3 , 10 y -1; el programa debería imprimir 4.67.

## 2.4 Tiempo de viaje

Un viajero desea saber cuánto tiempo tomó un viaje que realizó. Tiene la duración en minutos de cada uno de los tramos del viaje.

Desarrolle un programa **tiempo.c** que permita ingresar los tiempos de viaje de los tramos y entregue como resultado el tiempo total de viaje en formato horas:minutos.

El programa deja de pedir tiempos de viaje cuando se ingresa un 0.

~~~
Duracion tramo: 15
Duracion tramo: 30
Duracion tramo: 87
Duracion tramo: 0
Tiempo total de viaje: 2:12 horas
~~~

~~~
Duracion tramo: 51
Duracion tramo: 17
Duracion tramo: 0
Tiempo total de viaje: 1:08 horas
~~~


