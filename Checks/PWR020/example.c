// PWR020: consider loop fission to enable vectorization

void example() {
  int A[1000], B[1000], C[1000];

  for (int i = 0; i < 1000; i++) {
    A[i] = B[i] = C[i] = i;
  }

  for (int i = 0; i < 1000; i++) {
    A[i] += i;
    B[C[i]] += i;
  }
}
