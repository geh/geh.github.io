#define T 50 
#include <stdio.h>
int main(int argc, char ** argv){
    int cin[2*T+1]={0};
    int cab=T;
    int pas=0;
    int b=0;
    int d;

   
    for(b=0; pas<T; b=b+2, pas++){
      if(argv[1][b]==0){
      b=0;
      }
      for(d=0; d<=2*T; d++){
        printf("%d", cin[d]);
      }


      if(cin[cab]==1)
        b=b+1;
      
        if(argv[1][b]=='i'){
          cin[cab]=1;
          cab=cab-1;
        }else if(argv[1][b]=='I'){
                cin[cab]=1;
                cab=cab+1;
              }
        else if(argv[1][b]=='o'){
               cin[cab]=0;
               cab=cab-1;
             }
        else if(argv[1][b]=='O'){
               cin[cab]=0;
               cab=cab+1;
             }
        else if(argv[1][b]=='f'){
               pas=pas+1;
        printf("Instruccion %c\n", argv[1][b]);
               printf("Fin de ejecucion en %d pasos.\n", pas);
               return 0;
             }
        printf("Instruccion %c\n", argv[1][b]);

       if(b % 2 !=0)
         b=b-1;

    }if(pas==T)      
       printf("Fin de ejecucion por limite de tiempo %d pasos.\n", pas);

return 0;
}
