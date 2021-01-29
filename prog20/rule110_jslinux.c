#include <stdio.h>
#include <stdlib.h>

#define SIZE 72

main(){
    int i;
    int line[SIZE], next[SIZE];
    int rule110[8] = {0,1,1,1,0,1,1,0};
    // initialize first line with 1 bit on
    for(i=0; i < SIZE; i++) line[i]=0; line[SIZE/2]=1;
    // main loop
    while (1) {
        // print line
        for (i=0; i < SIZE; i++) printf("%d", line[i]); printf("\n");
        // evolve line
        next[0] = rule110[line[SIZE-1]*4 + line[0]*2 + line[1]];
        for(i=1; i < SIZE; i ++)
          next[i] = rule110[line[i-1] * 4 + line[i] *2 + line[(i+1)%SIZE]];
        for(i=0; i< SIZE; i++) line[i] = next[i];
        usleep(100000);
    }
}
