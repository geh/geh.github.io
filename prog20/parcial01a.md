% Programación 1: parcial 1 - parte 1
% 2020-05-11

La nota total de este parcial - sobre 10 puntos - ya contiene
3 puntos de participación y entrega de las tareas semanales.

Criterios de regularidad:

* tener por lo menos 1 punto en la parte "pruebas de escritorio"
* tener por lo menos 2 puntos en la parte "programas"

# Pruebas de escritorio (3 puntos)

Hacé las pruebas de escritorio siguientes por escrito, luego sacales fotos con tu celular y
mandales a <guillaumh@gmail.com> ni bien terminás.

## 1.1

~~~C
int a = 10;
int b = 1;
if (a <= 10 && b >= 1){
    b = b + 1;
}
if (a < 10){
    b = b + 1;
}
if (b > 1){
    b = b + 1;
} 
~~~


## 1.2

~~~C
int x = 9;
int c = 0;
if (x >= 8){
    c = c + 1;
} else if (x >= 9) {
    c = c + 10;
} else if (c >= 2) {
    c = c + 10;
}
~~~

## 1.3

~~~C
int a = 0;
int b = 9;
if (b > 10){
    a = 1;
} else if (b > 5){
    a = 2;
} else {
    a = 3;
}
~~~

## 1.4

~~~C
int i = 5;
int a = 0;
do{
    if (i % 2 == 0)
        a = a + 1;
    else
        a = a - 1;
} while (i < 4);
~~~

## 1.5

~~~C
int a = 0;
int i = 0;
if(i >= a){
  while (i < 9){
      a = a + i;
      i = 10;
  }
} else {
  i = i + 1;
}
~~~

## 1.6

~~~C
int i = 1;
int a = 1;
while (i < 7){
    a = a + i;
    i = a;
}
~~~


