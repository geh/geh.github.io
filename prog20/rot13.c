#define T 60

rot13_minus

int minuscula(char c){

 return 1;
}

int mayuscula(char c){

 return 1;
}


void rot13(char string[]){
 int i;
 char c;
 for(i=0; i<T; i++){
    c = string[i];
    if (c == 0) return; // fin de la cadena
    if (minuscula(c))
      printf("%c", rot13_minus(c));
    else if (mayuscula(c))
      printf("%c", rot13_mayus(c));
    else
      printf("%c", c);
 }
}


main(int argc, char * argv[]){
  char string[T]; 
  scanf("%s", string);
  rot13(string);
}
