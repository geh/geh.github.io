#include <stdio.h>
#include <stdlib.h>

typedef struct node {
 int data;
 struct node * next;
} Node, * NodePtr;

NodePtr newNode(int n){
 NodePtr np = (NodePtr) malloc(sizeof(Node));
 np -> data = n;
 np -> next = NULL;
 return np;
}

void printList(NodePtr top){
 NodePtr np;
 while( top != NULL ){
  printf("%d,", top -> data); 
  top = top -> next;
 }
}

main(){
 NodePtr top = NULL, np, last;
 int n;
 do{
   scanf("%d", &n);
   if(n != 0){
      np = newNode(n);
      if ( top == NULL ) top = np;
      else last->next = np;
      last = np;
   }
 } while(n != 0);
 printList(top);
} 
