// PWD007: Unprotected multithreading recurrence

void example() {
  int x[5], y[5];

  y[0] = 0;

  #pragma omp parallel for
  for (int i = 1; i < 5; i++) {
    y[i] = y[i - 1] + x[i - 1];
  }
}
