// PWR022: move invariant conditional out of the loop to facilitate
// vectorization

void solution(int n, const double *A, const double *B, const double *C,
              double *D) {
  for (int j = 0; j < n; ++j) {
    for (int k = 0; k < n; ++k) {
      D[j * n + k] = B[j * n + k] + A[0] * C[j * n + k];
    }
  }
  for (int i = 1; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      for (int k = 0; k < n; ++k) {
        D[j * n + k] = D[j * n + k] + A[i] * C[i * n * n + j * n + k];
      }
    }
  }
}
