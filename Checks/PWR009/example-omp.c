// PWR009: Use OpenMP teams to offload work to GPU

void example(double (*A)[100], double (*B)[100], double (*C)[100]) {
  #pragma omp target map(to: A[0:100][0:100], B[0:100][0:100]) \
          map(tofrom: C[0:100][0:100])
  {
    #pragma omp parallel default(none) shared(A, B, C)
    {
      #pragma omp for schedule(auto)
      for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            for (int k = 0; k < 100; k++) {
              C[i][j] += A[i][k] * B[k][j];
            }
        }
      }
    } // end parallel
  } // end target
}
