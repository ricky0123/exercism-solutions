#include "difference_of_squares.h"
#include <stdio.h>
#include <math.h>


unsigned int sum_of_squares(unsigned int number) {
  unsigned int out = 0;
  for (unsigned int i = 1; i <= number; i++) {
      out += pow(i, 2);
  }
  return out;
}

unsigned int square_of_sum(unsigned int number) {
  unsigned int s = 0;
  for (unsigned int i = 1; i <= number; i++) {
    s += i;
  }
  return pow(s, 2);
}

unsigned int difference_of_squares(unsigned int number) {
  return square_of_sum(number) - sum_of_squares(number);
}

