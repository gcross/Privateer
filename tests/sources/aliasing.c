#include <stdio.h>
#include <string.h>

int a = 12;
int *b = &a;

int main() {
  __initialize__();
  a = 21;
  printf("*b = %i",*b);
}
