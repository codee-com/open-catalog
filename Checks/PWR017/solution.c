// PWR017: Using while loops instead of for loops may inhibit vectorization

__attribute__((pure)) int solution(int *A) {
  int sum = 0;
  for (int i = 0; i < 1000; i++) {
    sum += A[i];
  }
  return sum;
}
