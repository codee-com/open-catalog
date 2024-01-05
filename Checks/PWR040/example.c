// PWR040: Consider loop tiling to improve the locality of reference

void example(double **A, double **B, int n) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      A[i][j] = B[j][i];
    }
  }
}
