# PWR013: Avoid copying unused variables to or from the GPU

### Issue

Unused variables should never be copied to or from the GPU to prevent
unnecessary [data movements](/Glossary/Offloading.md) between the CPU and the
GPU, which impacts performance.

### Relevance

One of the key challenges when offloading work to the GPU is minimizing the data
transfers between CPU memory and GPU memory. These transfers can greatly affect
performance and should be minimized. Thus, only the strictly required data
should be copied to or from the GPU memory.

### Actions

Remove the unused variables from the data mapping clauses.

### Code example

In the following example, matrix `B` is copied to the GPU even when it is not
used:

```c
void example(double *A, double *B, double *C) {
  #pragma omp target teams distribute parallel for schedule(auto) shared(A, B) \
          map(to: A[0:100], B[0:100]) map(tofrom: C[0:100])
  for (int i = 0; i < 100; i++) {
    C[i] += A[i];
  }
}
```

This can be easily corrected by removing references to B from all the clauses:

```c
void example(double *A, double *B, double *C) {
  #pragma omp target teams distribute parallel for schedule(auto) shared(A) \
          map(to: A[0:100]) map(tofrom: C[0:100])
  for (int i = 0; i < 100; i++) {
    C[i] += A[i];
  }
}
```

### Related resources

* [PWR013 examples at GitHub](/Checks/PWR013)
