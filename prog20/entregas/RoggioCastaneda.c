#include <stdio.h>
#define T 50

void imprimir_cinta(int cinta[T*2+1]);

int main(int argc, char*argv[]){
    int i=0;
    int cabezal=T;
    int cinta[T*2+1]={0};
    int cont_instrucciones=0;

    while (cont_instrucciones<T && argv[1][i]!='f'){
        if(argv[1][i]=='i'){
            cinta[cabezal]=1;
            cabezal=cabezal - 1;
        }else if(argv[1][i]=='I'){
            cinta[cabezal]=1;
            cabezal=cabezal + 1;
        }else if(argv[1][i]=='o'){
            cinta[cabezal]=0;
            cabezal=cabezal - 1;
        }else if(argv[1][i]=='O'){
            cinta[cabezal]=0;
            cabezal=cabezal + 1;
        }

        imprimir_cinta(cinta);
        printf(" instruccion: %c\n", argv[1][i]);
        cont_instrucciones++;
        i++;
        if (argv[1][i]=='\0'){
            i=0;
        }
    }
    printf("fin de ejecucion en: %d pasos\n", cont_instrucciones);
}



void imprimir_cinta(int cinta[T*2+1]){
    int i;
    for (i=0;i<(T*2+1);i++)
        printf("%d", cinta[i]);

}


