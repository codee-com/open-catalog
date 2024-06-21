// PWD007: Unprotected multithreading recurrence

void example(int *x, int *y, int size) {
  y[0] = 0;

  #pragma omp parallel for
  for (int i = 1; i < size; i++) {
    y[i] = y[i - 1] + x[i - 1];
  }
}
