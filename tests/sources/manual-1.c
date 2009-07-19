#include <stdio.h>

int i = 1;
static int j = 2;
extern int k;

int main() {
  __initialize__();
  printf("i = %i, j = %i",i,j);
}
