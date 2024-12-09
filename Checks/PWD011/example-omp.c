// PWD011: Missing OpenMP lastprivate clause

double example(int m, double *A, double *B, double *C) {
  double liveOut;

  // liveOut is private but used after the loop, so it should be lastprivate
  #pragma omp parallel for private(liveOut) shared(A, B, C)
  for (int i = 0; i < m; i++) {
    liveOut = A[i] * B[i];
    C[i] = C[i] + liveOut;
  }

  liveOut += 5;
  return liveOut;
}
