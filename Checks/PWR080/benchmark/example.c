// PWR080: Conditionally initialized variables can lead to undefined behavior

__attribute__((pure)) double transform_and_sum(const int n, const double *array,
                                               const int option) {
  double sum = 0.0;

  double factor;
  if (option == 1) {
    factor = 0.5;
  } else if (option == 2) {
    factor = 2.0;
  }

  for (int i = 0; i < n; ++i) {
    sum += array[i] * factor;
  }

  return sum;
}
