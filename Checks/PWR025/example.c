// PWR025: consider annotating pure function with OpenMP ‘declare simd’

int foo(int a) {
  return 2 * a;
}

void example(int *A, int n) {
  for (int i = 0; i < n; i++) {
    A[i] = foo(i);
  }
}
