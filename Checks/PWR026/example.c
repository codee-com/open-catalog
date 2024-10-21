// PWR026: Annotate function for OpenMP offload

int foo(int a) {
  return 2 * a;
}

void example(int n, int *A) {
  #pragma omp target teams distribute parallel for default(none) shared(A, n)
  for (int i = 0; i < n; i++) {
    A[i] = foo(i);
  }
}
