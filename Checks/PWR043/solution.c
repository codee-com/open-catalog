// PWR043: Loop nest can benefit from loop interchange, but reduction variable
// initialization prevents loop interchange

void matmul_improved(int n, const double *A, const double *B, double *C) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      C[i * n + j] = 0.0;
    }
  }
  for (int i = 0; i < n; ++i) {
    for (int k = 0; k < n; ++k) {
      for (int j = 0; j < n; ++j) {
        C[i * n + j] += A[i * n + k] * B[k * n + j];
      }
    }
  }
}
