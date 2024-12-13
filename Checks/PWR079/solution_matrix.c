// PWR079: Avoid undefined behavior due to uninitialized variables

#include <stdio.h>

void perform_computation(double *matrix_a, double *matrix_b) {
  if (matrix_a == NULL || matrix_b == NULL) {
    printf("A matrix is NULL; skipping computation\n");
    return;
  }

  printf("Performing computation...\n");
}

int main() {
  double *matrix_a = NULL, *matrix_b = NULL;
  perform_computation(matrix_a, matrix_b);

  return 0;
}
