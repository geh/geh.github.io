#define T 50
#include <stdio.h>
#include <stdlib.h>


int main(int argc, char *argv[]){
char *c = argv[1];
int i = 0;
int cinta[T*2+1]={0};
int cabezal=T;
int contador = 0;
int pasos = 0;

printf("Trabajo MTBC Fracisco Asis, Roberto Camacho, Elias Diaz. \n");


  for(contador=0;contador<T*2+1;contador++){
    printf("%d",cinta[contador]);
     }
  pasos = pasos + 1;
     printf("\n");

void comparaf(){
	if(c[i]=='f'){
          cabezal = cabezal + T*2+1;   
          for(contador=0;contador<T*2+1;contador++){
            printf("%d",cinta[contador]);
            }
          printf(" Instruccion 'f'. \n"); 
    }
}

void comparao(){         
        if(c[i]=='o'){
            cinta[cabezal]=0;
            cabezal=cabezal-1;
            pasos = pasos + 1;
            for(contador=0;contador<T*2+1;contador++){
            printf("%d",cinta[contador]);
          }

       printf(" Instruccion 'o'.\n");
       }
}

void comparaO(){
      if(c[i]=='O'){
           cinta[cabezal]=0;
           cabezal=cabezal +1;
           pasos = pasos + 1;
           for(contador=0;contador<T*2+1;contador++){
            printf("%d",cinta[contador]);
            }

      printf(" Instruccion 'O'.\n");
      }
}

void comparai(){
      if(c[i]=='i'){
           cinta[cabezal]=1;
           cabezal=cabezal -1;
           pasos = pasos + 1;
           for(contador=0;contador<T*2+1;contador++){
           printf("%d",cinta[contador]);
           } 
      printf(" Instruccion 'i'.\n");
      }
}

void comparaI(){

      if(c[i]=='I'){
           cinta[cabezal]=1;
           cabezal=cabezal+1;
           pasos = pasos + 1;
           for(contador=0;contador<T*2+1;contador++){
           printf("%d",cinta[contador]);
           }
      printf(" Instruccion 'I'.\n");
      }
}

do{
 comparaf();
 comparai();
 comparaI();
 comparao();
 comparaO();

    i = i + 1;      

  }while(cabezal<T*2+1 && cabezal>0);



  if(pasos>=50){
  printf("excedio los limites de pasos, limite de pasos 50.\n");
  }else{
  printf("fin de la ejecucion,cantidad de %d pasos.", pasos);
  }


}

