#include <stdio.h>
#include <string.h>
#define T 50

int main(int argc,char *argv[]){

int c = 0, cab = T, i = 0;
int arr[T*2+1]={0};
int pos = 0;


char letra = argv[1][pos];
    while(c < T && letra != 'f'){

        if(letra == 'i'){
            arr[cab]=1;
            cab=cab-1;

        }else if(letra=='I'){
            arr[cab]=1;
            cab=cab+1;

        }else if(letra=='O'){
            arr[cab]=0;
            cab=cab+1;

        }else if(letra=='o'){
            arr[cab]=0;
            cab=cab-1;

        }          
        for(i = 0;i < (T*2+1);i++){
            printf("%d",arr[i]);
        }

        printf(" instruccion %c\n",letra);
        c = c + 1;

	
	pos = pos + 2;
	if(argv[1][pos] == 0) pos = 0;

        if (arr[cab]==0){
	  letra = argv[1][pos];
        } else {
	  letra = argv[1][pos+1];
        }

        }
        printf("fin de ejecucion en %d pasos\n",c);
        return 1;
        }


