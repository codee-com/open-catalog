// PWR060: consider loop fission to separate gather memory access pattern

void example(double a, double *D, double *X, double *Y, int *index, int n) {
  for (int i = 0; i < n; ++i) {
    D[i] = a * X[index[i]] + Y[i];
  }
}
