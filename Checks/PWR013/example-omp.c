// PWR013: Avoid copying unused variables to the GPU

void example(double *A, double *B, double *C) {
  #pragma omp target teams distribute parallel for schedule(auto) shared(A, B) \
      private(i) map(to: A[0:100], B[0:100]) map(tofrom: C[0:100])
  for (int i = 0; i < 100; i++) {
    C[i] += A[i];
  }
}
