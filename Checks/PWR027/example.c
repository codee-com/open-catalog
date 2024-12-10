// PWR027: Annotate function for OpenACC offload

__attribute__((const)) int foo(int a) {
  return 2 * a;
}

void example(int n, int *A) {
  #pragma acc kernels
  for (int i = 0; i < n; i++) {
    A[i] = foo(i);
  }
}
