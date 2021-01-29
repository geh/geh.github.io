#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv)
{   int i=0;
    int k=0;
    int primera;
    int segunda;
    int tercera;
    int cuarta;
    int quinta; 
    int suma;
    int promedio;
    int mayor=0; 
    int n;
    int u=0;
    int x=0;
    int prom;
  
    while (i< 25){
        k = k + 11;
        printf("%d\n",k);
        i++;
       }
     printf("Ingrese la altura en cm\n");
        scanf("%d", &primera);
        scanf("%d", &segunda);
        scanf("%d", &tercera);
        scanf("%d", &cuarta);
        scanf("%d", &quinta);
     suma = primera + segunda + tercera + cuarta + quinta;  
     promedio = (suma / 5);
     printf("El promedio es %d\n", promedio);
     if (primera > 150)
         mayor = mayor + 1;
     if (segunda > 150)
         mayor = mayor + 1;
     if (tercera > 150)
         mayor = mayor + 1;
     if (cuarta > 150)
         mayor = mayor + 1;
     if (quinta > 150)
         mayor = mayor + 1;
     printf("Las altures mayores a 150 son %d\n", mayor);
    
    do{
       printf("Ingrese una serie de nuemros entre 0 y 100\n");
       scanf("%d", &n);
       if (n >= 0 && n <=100){
          u = n + u;
          x++;
           
       }
    } 
    while (n >= 0 && n <=100);
 
     printf("Suma:%d\n", u); 
     prom = ( u / x);
     printf("El promedio es %d\n", prom);
     
 
     return 0;
  
     
}
