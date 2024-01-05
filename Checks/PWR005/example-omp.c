// PWR005: Disable default OpenMP scoping

void example(int *result, unsigned size) {
  // Default data scoping is used which may not be correct
  #pragma omp parallel for
  for (int i = 0; i < size; i++) {
    result[i] = i;
  }
}
