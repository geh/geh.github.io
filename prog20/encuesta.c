#define T 5
main(){
 int cal[T] = {0};
 int n;
 int i;
 int c;

 printf("Cuantos alumnos van a contestar a la encuesta?\n");
 scanf("%d", &n);

 for(i=1 ; i<=n; i=i+1){

   do{
     printf("Alumno %d, ingrese su voto (1-5):\n", i);
     scanf("%d", &c);
  } while (c < 1 && c > 5);

   cal[c-1] = cal[c-1] + 1;
 }

 printf("\nResultados:\n\n");

 for(i=1; i<=5; i++){
   printf("Calificacion %d : %d voto(s)\n", i, cal[i-1]);
 }
   
}
