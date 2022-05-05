#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define Max = 100

main(int argc, char **argv){
  assert(argc  > 1);
  char left, symbol, right;
  int P[Max+1];  //P used to implement disjoint subsets
  FILE * in = fopen(argv[1], "r");

  while (fscanf(in, "%c %c %c", &left, &symbol, &right) == 1){




  }



}
