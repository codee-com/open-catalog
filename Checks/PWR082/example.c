// PWR082: Remove unused variables

__attribute__((pure)) int sumArray(const int n, const int *array) {
  int t, sum = 0;

  for (int i = 0; i < n; ++i) {
    sum += array[i];
  }

  return sum;
}
