#define T 5
main(){
 char a[T];
 char b[T];
 char i;
 char s=0;
 char may=0;
 char men=100;
 srand(time(0));
 printf("[");
 for(i=0;i<T;i++)
  printf("%d%s", a[i]=rand()%10, i==T-1 ? "]\n": ",");
 for(i=0;i<T;i++){
    s = s + a[i];
    if ( a[i] > may ) may = a[i];
    if ( a[i] < men ) men = a[i];
 }
 printf("Suma: %d\n", s);
 printf("Promedio: %d\n", s/T);
 printf("Maximo: %d\n", may);
 printf("Minimo: %d\n", men);
 // veamos si esta ordenada
 s=1; i=0;
 while(s==1 && i < T- 2){
     if (a[i] > a[i+1]) s = 0; else i = i+1;
 }
 if(s==1) printf("La lista esta ordenada.\n");
 else printf("La lista no esta ordenada porque %d > %d.\n", a[i], a[i+1]);
 
}
