// PWR005: Disable default OpenMP scoping

void example(int *result, unsigned size) {
  // NOT-PWR002: Declaration outside loop to trigger PWR005
  int t;

  // Default data scoping is used, making `t` shared instead of private
  #pragma omp parallel for
  for (int i = 0; i < size; i++) {
    t = i + 1;
    result[i] = t;
  }
}
