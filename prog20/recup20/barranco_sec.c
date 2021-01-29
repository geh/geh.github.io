#include <stdio.h>
#include <stdlib.h>
int main(){
int a = 0;
int i = 0;
 while(i < 25){
 a = a + 11;
  printf("%d\n", a);
 
 i++;
}

 int b;
 int c;
 int d;
 int e;
 int f;
 int suma = 0;
 int promedio = 0;
 int k = 0;
 printf("ingresar 5 alturas en centimetros\n");
  scanf("%d", &b);
  scanf("%d", &c);
  scanf("%d", &d);
  scanf("%d", &e);
  scanf("%d", &f);
 
 suma = suma + b + c + d + e + f;
 
 promedio = suma / 5;

 printf("el promedio de altura es de %d\n", promedio);  

 if( b > 150){
 k++;
}
 if( c > 150){
 k++;
}
 if( d > 150){
 k++;
} 
 if ( e > 150){
 k++;
} 
 if( f > 150){
 k++;
}

 printf("hay %d alturas mayores a 150cm\n", k);

 int z;
 int valores = 0;
 int x = 0;
 int w = 0;
 int promedios = 0;


 printf("ingrese valores entre 0 y 100\n");
 do{ 
 
  scanf("%d", &z);
 valores = valores + z;
 x++;  
} 
 while( z <= 100);
printf("el valor no esta dentro del rango\n");

 w = w + z; 

 promedios = w / x;

 printf("el promedio de numeros que ingreso es %d\n", promedios);















return 0;
}
