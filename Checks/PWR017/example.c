// PWR017: Using while loops instead of for loops may inhibit vectorization

int example(int *A) {
  int sum = 0;
  int i = 0;
  while (i < 1000) {
    sum += A[i++];
  }
  return sum;
}
