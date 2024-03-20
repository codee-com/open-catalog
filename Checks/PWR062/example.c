// PWR062: Consider loop interchange by removing accumulation on array value

void matmul(int n, const double *A, const double *B, double *C) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      C[i * n + j] = 0.0;
      for (int k = 0; k < n; ++k) {
        C[i * n + j] += A[i * n + k] * B[k * n + j];
      }
    }
  }
}
