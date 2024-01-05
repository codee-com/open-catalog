// PWR014: Out-of-dimension-bounds array access

void example() {
  int A[100][100];
  for (int i = 1; i < 100; ++i) {
    for (int j = 0; j < 100; ++j) {
      A[i][j - 1] = 1;
    }
  }
}
