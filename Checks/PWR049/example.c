// PWR049: Move iterator-dependent condition outside of the loop

void example(int **a, int **b, int n) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      if (j == 0) {
        a[i][j] = 0;
      } else {
        a[i][j] = a[i][j - 1] + b[i][j];
      }
    }
  }
}
