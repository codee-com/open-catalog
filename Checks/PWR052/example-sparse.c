// PWR052: Consider applying multithreading parallelism to sparse reduction loop

void example(double *A, int *nodes, int n) {
  for (int nel = 0; nel < n; ++nel) {
    A[nodes[nel]] += nel * 1;
  }
}
