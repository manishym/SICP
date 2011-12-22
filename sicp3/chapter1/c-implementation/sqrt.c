#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define EPS 0.0001
float average (float x, float y) ;
float improve (float guess, float x) ;
int good_enough (float guess, float x) ;
float my_square_root (float x, float guess) ;
float square (float x) ;
int main (int argc, char* argv[]) {

  float x  = atof(argv[1]) ;
  if (!x)
    return -1 ;
  printf ("%f\n", my_square_root(x, 1.0)) ;
  return 0;
}

float my_square_root (float x, float guess) {
  if (good_enough(guess, x) ) {
      return guess ;
  }
  else { 
    return my_square_root (x, improve (guess, x)) ;
  }
}

int good_enough (float guess, float x) {
  if (fabsf ( square (guess) - x) < EPS)
    return 1 ;
  else 
    return 0 ;
}

float improve (float guess, float x) {
  float improved ;
  return improved = average (guess, x/guess) ;
}

float average (float x, float y) {
  return ( x + y ) / 2 ;
}
float square (float x)
{
  return (x * x) ;
}
