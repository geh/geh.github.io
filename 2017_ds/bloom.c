// Implement a program that takes an input file,
// then prompts the user for words, and tells each time
// whether the word belongs to the file.

// To do so, implement a Bloom Filter with an array of char
// for which we only use 0 or 1 values (inefficient, but
// this way we do not do any bitwise operations).
// Make the char array 10000 (LEN) bits big.

#define LEN 10000

// When reading the input file, count the number of words.
// If the number of word is more than LEN/10, stop
// adding words to the Bloom Filter.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define MaxWordSize 30

/* D. J. Bernstein hash function */
int djb_hash(const char* cp)
{
    int hash = 5381;
    while (*cp)
        hash = 33 * hash ^ (unsigned char) *cp++;
    return hash;
}

/* Fowler/Noll/Vo (FNV) hash function, variant 1a */
int fnv1a_hash(const char* cp)
{
    int hash = 0x811c9dc5;
    while (*cp) {
        hash ^= (unsigned char) *cp++;
        hash *= 0x01000193;
    }
    return hash;
}

int getWord(FILE * in, char str[]) {
// stores the next word, if any, in str; word is converted to lowercase
// returns 1 if a word is found; 0, otherwise
   char ch;
   int n = 0;
   // read over non-letters
   while (!isalpha(ch = getc(in)) && ch != EOF) ;
   if (ch == EOF) return 0;
   str[n++] = tolower(ch);
   while (isalpha(ch = getc(in)) && ch != EOF)
      if (n < MaxWordSize) str[n++] = tolower(ch);
   str[n] = '\0';
   return 1;
}

int main(){
    char bloom[LEN] = {0};
    FILE * in = fopen("passage.in", "r");
    int numWords = 0;
    char word[MaxWordSize+1];

    while (getWord(in, word) != 0) {
          if (numWords < (LEN/10)) {
             numWords++;
             // ... insert into Bloom Filter ... //
          }
          else printf("'%s' not added to filter\n", word);
    }

    printf("Done building bloom filter, %d words added.", numWords);

    while (scanf("%s", word) == 1){
       // ... check for presence of the word in Bloom Filter, //
       //     output accordingly ... //
    }

    fclose(in);
    return 0;
}
