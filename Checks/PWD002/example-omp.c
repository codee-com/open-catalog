// PWD002: Unprotected multithreading reduction operation

void example(int *array) {
  int sum = 0;

  #pragma omp parallel for default(none) shared(array, sum)
  for (int i = 0; i < 10; i++) {
    sum += array[i];
  }
}
