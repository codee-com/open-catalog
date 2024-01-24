// PWD005: Array range copied to the GPU does not cover the used range

void example() {
  int A[100], B[100], sum[100];

  #pragma omp target map(to: A[0:50], B[0:50]) map(from: sum[0:50])
  #pragma omp parallel for
  for (int i = 0; i < 100; i++) {
    sum[i] = A[i] + B[i];
  }
}
