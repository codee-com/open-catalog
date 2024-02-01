// PWR022: move invariant conditional out of the loop to facilitate
// vectorization

void example(int n, const double *A, const double *B, const double *C,
             double *D) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      for (int k = 0; k < n; ++k) {
        const double val = i == 0 ? B[j * n + k] : D[j * n + k];
        D[j * n + k] = val + A[i] * C[i * n * n + j * n + k];
      }
    }
  }
}
