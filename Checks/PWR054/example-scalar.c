// PWR054: consider applying vectorization to scalar reduction loop

__attribute__((pure)) double example(double *A, int n) {
  double sum = 0.0;

  for (int i = 0; i < n; ++i) {
    sum += A[i];
  }

  return sum;
}
