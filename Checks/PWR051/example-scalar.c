// PWR051: Consider applying multithreading parallelism to scalar reduction loop

// NOT-PWR003: OpenMP parallelization pragmas aren't considered `pure`
double example(double *A, int n) {
  double sum = 0.0;

  for (int i = 0; i < n; ++i) {
    sum += A[i];
  }

  return sum;
}
