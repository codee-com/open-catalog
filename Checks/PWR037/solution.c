// PWR037: Potential precision loss in call to mathematical function

#include <math.h>
#include <stdio.h>

__attribute__((const)) double square_root(const double value) {
  return sqrt(value);
}

int main() {
  printf("Square root of 2 is: %0.15f\n", square_root(2.0));
}
