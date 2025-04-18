// PWR025: Consider annotating pure function with OpenMP `declare simd`

__attribute__((const)) int foo(int a) {
  return 2 * a;
}

void example(int *A, int n) {
  for (int i = 0; i < n; i++) {
    A[i] = foo(i);
  }
}
