// PWR005: Disable default OpenMP scoping

void example(int *result, unsigned size) {
  int t;

  // Default data scoping is used, making `t` shared instead of private
  #pragma omp parallel for
  for (int i = 0; i < size; i++) {
    t = i + 1;
    result[i] = t;
  }
}
