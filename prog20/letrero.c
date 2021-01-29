#define T 15
main(){
 char l[T] = "LA CUMBRE     ";
 char primero;
 char i;

 while(1){
   system("clear");
   printf("%s\n", l);

   primero = l[0];
   for(i=1;i<T-1;i++) l[i-1]=l[i];
   l[T-2] = primero;
   usleep(100000);
 }


}
