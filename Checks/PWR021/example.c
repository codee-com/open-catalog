// PWR021: temporary computation can be extracted to a vectorizable loop

int expensive_computation(int *C, int i) {
  return C[i] * 2;
}

void example(int *A, int *C) {
  for (int i = 0; i < 1000; i++) {
    int t = expensive_computation(C, i);
    A[C[i]] += t;
  }
}
