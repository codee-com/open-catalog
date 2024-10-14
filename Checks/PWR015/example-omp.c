// PWR015: Avoid copying unnecessary array elements to the GPU

void example() {
  int A[100], B[100], sum[100];
  #pragma omp target map(to: A[0:100], B[0:100]) map(from: sum[0:100])
  #pragma omp parallel for private(i)
  for (int i = 0; i < 50; i++) {
    sum[i] = A[i] + B[i];
  }
}
