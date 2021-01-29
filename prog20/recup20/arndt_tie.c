#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv)
{   
    int tiempos;
    int suma=0;
    int hora=0;
    int minutos=0;
   
    do {
    printf("Ingrse los tiempos\n");
    scanf("%d", &tiempos);
    suma = tiempos + suma;
   }
    while(tiempos != 0);
   
    hora = (suma / 60);
    minutos = ((suma - hora) *60);
      
    printf("tiempo total de viaje %d:", hora);
    printf("%d horas", minutos);
    
    return 0;
}
