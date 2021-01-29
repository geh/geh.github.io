#include <stdio.h>
#include <stdlib.h>
int main(){
int a; 
int suma = 0;
int horas = 0;
int minutos = 0;
 printf("ingrese el tiempo que demoro en su viaje expresado en minutos");
 do{ 
 scanf("%d", &a);

 if(a > 60){
  horas = a / 60;
} 
else{
 minutos = minutos + a;
 }
}
while( a > 0); 
 printf("el viaje demoro %d horas", horas); printf(":%d minutos", minutos);


 
 return 0;
}  
