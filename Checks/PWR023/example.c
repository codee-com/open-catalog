// PWR023: Add 'restrict' for pointer function arguments to hint the compiler
// that vectorization is safe

int example(int *x, int *y) {
  int sum = 0;
  for (int i = 0; i < 10; ++i) {
    sum += x[i];
    y[i] = 2;
  }
  return sum;
}
