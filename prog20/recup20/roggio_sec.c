#include <stdlib.h>
#include <stdio.h>

main(){
  int suma = 0;
  int incremento = 0;
  int altura;
  int mayor = 0;
  int promedio = 0;
  int entero = 1;
  while (incremento < 25){
    suma = suma + 11;
    printf ("%d ", suma);
    incremento++;
  }
  printf("\n\n");

  incremento = 0;
  suma = 0;

  printf ("Ingrese 5 alturas en centimetros\n");

  while (incremento < 5){
    scanf ("%d", &altura);
    suma = altura + suma;
    if (altura >= 150)
        mayor++;

    incremento++;
  }

  promedio = suma/incremento;
  if (mayor == 1)
    printf("%d persona  mide mas de 150cm\n", mayor);
  else
    printf ("%d personas miden mas de 150cm\n", mayor);

  printf ("El promedio es %d\n\n", promedio);

  suma = 0;
  incremento = 0;
  promedio = 0;

  printf ("Ingrese valores enteros\n");

  while ( entero >= 0 && entero < 100){
    scanf ("%d", &entero);
    if (entero > 0 && entero < 100){
      suma = entero + suma;
      incremento++;
    }
  }
  promedio = suma/incremento;

  if (promedio >= 0 && promedio < 100)
        printf (" El promedio de todos los valores ingresados es: %d", promedio);

}
