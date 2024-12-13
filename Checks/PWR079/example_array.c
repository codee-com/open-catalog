// PWR079: Avoid undefined behavior due to uninitialized variables

#include <stdio.h>

__attribute__((pure)) double sum_array(double *array, size_t size) {
  double sum;

  for (size_t i = 0; i < size; ++i) {
    sum += array[i];
  }

  return sum;
}

int main() {
  double array[] = {0.24, 0.33, 0.17, 0.89, 0.05};
  printf("Sum is: %f\n", sum_array(array, 5));

  return 0;
}
