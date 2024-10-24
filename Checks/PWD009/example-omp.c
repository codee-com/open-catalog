// PWD009: Incorrect privatization in parallel region

void example(int m, double *A, double *B, double *C) {
  double temp;

  // "C" should be shared
  #pragma omp parallel for default(none) private(i, temp, C) shared(A, B, m)
  for (int i = 0; i < m; i++) {
    temp = A[i] * B[i];
    C[i] = C[i] + temp;
  }
}
