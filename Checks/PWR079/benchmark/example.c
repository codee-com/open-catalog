// PWR079: Avoid undefined behavior due to uninitialized variables

__attribute__((pure)) double sum_array(const int n, const double *array) {
  double sum;

  for (int i = 0; i < n; ++i) {
    sum += array[i];
  }

  return sum;
}
