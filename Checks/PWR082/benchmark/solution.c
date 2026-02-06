// PWR082: Remove unused variables

__attribute__((pure)) double sum_array_improved(const int n,
                                                const double *array) {
  double sum = 0.0;

  for (int i = 0; i < n; ++i) {
    sum += array[i];
  }

  return sum;
}
