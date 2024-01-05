// PWR024: loop can be rewritten in OpenMP canonical form

void example(int *A, int n, int m) {
  int i = 0;
  for (; i < n; i++) {
    A[i] = m;
  }
}
