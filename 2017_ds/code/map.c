int square(int a){
  return a*a;
}

int plusOne(int a){
 return a+1;
}

void map(int arr[], int size, int (*fp) (int)){
  for(int i=0; i < size ; i++)
   arr[i] = (*fp)(arr[i]);
}

main(){
 int a[10];
 for(int i =0; i < 10; i++ )  a[i] = rand() % 10;
 map(a, SIZE, plusOne);
 map(a, SIZE,  square);
}
