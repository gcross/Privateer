#include <stdio.h>

static int global_variable = 42;

static void hello_world() {
  printf("Hello, world! %i\n", global_variable);
}

int main() {
  __initialize__();
  hello_world();
}
