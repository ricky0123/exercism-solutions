#include "dnd_character.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

// typedef struct {
//    int strength;
//    int dexterity;
//    int constitution;
//    int intelligence;
//    int wisdom;
//    int charisma;
//    int hitpoints;
// } dnd_character_t;

int roll_dice(void);
int roll_dice(void) {
  srand(time(NULL));
  return (rand() % 6) + 1;
}

/*
 * void qsort( void* ptr, size_t count, size_t size,
 *                        int (*comp)(const void*, const void*) );
 *
*/

int comp(const void* a, const void* b);
int comp(const void* a, const void* b) {
  const int* first = (const int*)a;
  const int* second = (const int*)b;
  if (*first < *second) {
    return -1;
  } else if (*first == *second) {
    return 0;
  } else {
    return 1;
  }
}

int ability(void) {
  int *start = malloc(sizeof(int) * 4);
  for (int i = 0; i < 4; i++) {
    start[i] = roll_dice();
  }

  qsort(start, 4, sizeof(int), *comp);
  return (start[1] + start[2] + start[3]);
}

int modifier(int score) {
  // 3: 3 - 10 -> -7 / 2 -> -3.5 -> -4
  const float score_f = (float)score;
  const float res_f = floor((score_f - 10) / 2);
  return (int)res_f;
}

dnd_character_t make_dnd_character(void) {
  const int constitution = ability();
  const int _modifier = modifier(constitution);
  const int hitpoints = 10 + _modifier;
  return (dnd_character_t) {
    .strength = ability(),
    .dexterity = ability(),
    .constitution = constitution,
    .intelligence = ability(),
    .wisdom = ability(),
    .charisma = ability(),
    .hitpoints = hitpoints,
  };
}


