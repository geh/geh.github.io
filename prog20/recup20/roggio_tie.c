#include <stdlib.h>
#include <stdio.h>

main(){
  int tiempo;
  int suma = 0;
  int horas = 0;
  int minutos;
  do{ 
    printf ("Duracion del tramo:\n");
    scanf ("%d", &tiempo);
    suma = tiempo + suma;
    } while (tiempo > 0);
  if (suma > 60){
       horas = suma/60;
       minutos = suma - 60 * horas;

  }

  printf(" tiempo total de viaje: %d:", horas );
  printf ("%d horas", minutos);

}

