// PWR080: Conditionally initialized variables can lead to undefined behavior

#include <stdio.h>

typedef enum {
  OPTION_HALF,
  OPTION_DOUBLE,
  OPTION_UNKNOWN,
} TransformOption;

double transform_and_sum(const double *array, size_t size,
                         TransformOption option) {
  double sum = 0.0;

  double factor;
  if (option == OPTION_HALF) {
    factor = 0.5;
  } else if (option == OPTION_DOUBLE) {
    factor = 2.0;
  }

  for (size_t i = 0; i < size; ++i) {
    sum += array[i] * factor;
  }

  return sum;
}

int main() {
  double array[] = {0.25, 0.25, 0.25, 0.25};
  printf("Sum is: %f\n", transform_and_sum(array, 4, OPTION_UNKNOWN));

  return 0;
}
