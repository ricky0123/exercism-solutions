#include "rna_transcription.h"
#include <stdlib.h>

/*
 *    they're passing in a pointer to the first memory location in an array of memory locations, each of which hold a char (thus forming a string)
 *
 *    they want a pointer to a completely separate memory...
 *
 *
 *    1. figure out how many memory locations we need
           * keep incrementing "dna" pointer, incrementing an integer until we hit '\0'
 *    2. allocate the memory
           * malloc(sizeof(char) * L)
 *    3. loop simultaneously through both regions, choosing appropriate values for new string
           * ...
 */

int str_length(const char *str);

int str_length(const char *str) {
  int out = 0;
  const char *p = str;
  while (*p != '\0') {
    out++;
    p++;
  }
  return out;
}


char *to_rna(const char *dna) {
  int dna_size = str_length(dna);
  char *new_str = malloc(sizeof(char) * (dna_size + 1));
  char *out = new_str;

  while (*dna != '\0') {
    switch (*dna) {
      case 'G':
        *new_str = 'C';
        break;
      case 'C':
        *new_str = 'G';
        break;
      case 'T':
        *new_str = 'A';
        break;
      case 'A':
        *new_str = 'U';
        break;
    }
    dna++;
    new_str++;
  }
  *new_str = '\0';
  return out;
}

