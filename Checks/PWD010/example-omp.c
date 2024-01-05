// PWD010: Incorrect sharing in parallel region

void example(int **result, unsigned rows, unsigned cols) {
  int i, j;

  // j is implicitly shared and it should be private!
  #pragma omp parallel for shared(result)
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      result[i][j] = 0;
    }
  }
}
