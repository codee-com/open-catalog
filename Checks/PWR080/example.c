// PWR080: Conditionally initialized variables can lead to undefined behavior

#include <stdio.h>
#include <string.h>

double transform_and_sum(const double *array, size_t size, const char *option) {
  double sum = 0.0;

  double factor;
  if (strcmp(option, "half") == 0) {
    factor = 0.5;
  } else if (strcmp(option, "double") == 0) {
    factor = 2.0;
  }

  for (size_t i = 0; i < size; ++i) {
    sum += array[i] * factor;
  }

  return sum;
}

int main() {
  double array[] = {0.25, 0.25, 0.25, 0.25};
  printf("Sum is: %f\n", transform_and_sum(array, 4, "unknownOption"));

  return 0;
}
