// PWR046: Replace two divisions with a division and a multiplication

#include <stdio.h>

__attribute__((const)) float example(float a, float b, float c) {
  return a / (b * c);
}

int main() {
  printf("Result: %0.7f\n", example(1.23, 1.41, 1.89));
  return 0;
}
