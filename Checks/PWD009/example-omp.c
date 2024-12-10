// PWD009: Incorrect privatization in parallel region

void example(int m, double *A, double *B, double *C) {
  // "C" should be shared
  #pragma omp parallel for default(none) private(C) shared(A, B, m)
  for (int i = 0; i < m; i++) {
    double temp = A[i] * B[i];
    C[i] = C[i] + temp;
  }
}
