// PWD006: Missing deep copy of non-contiguous data to the GPU

void example(int **A) {
  #pragma omp target teams distribute parallel for map(tofrom: A)
  for (int i = 0; i < 10; i++) {
    A[i][i] += i;
  }
}
