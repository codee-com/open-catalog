# PWR015: Avoid copying unnecessary array elements to or from the GPU

### Issue

Unused data should never be copied to or from the GPU to prevent unnecessary
[data movements](/Glossary/Offloading.md) between the CPU and the GPU, which
impacts performance.

### Relevance

One of the key challenges when offloading work to the GPU is minimizing the data
transfers between CPU memory and GPU memory. These transfers can greatly affect
performance and should be performed only when needed. Thus, only the strictly
required data should be copied to or from the GPU memory.

### Actions

Restrict the array range to be copied to the GPU to the range strictly required.

### Code example

The following code performs the sum of two arrays:

```c
void example() {
  int A[100], B[100], sum[100];
  #pragma omp target map(to: A[0:100], B[0:100]) map(from: sum[0:100])
  #pragma omp parallel for
  for (int i = 0; i < 50; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

However, only half of the total array elements are actually being used. Thus,
there is no need to transfer the entire arrays:

```c
void example() {
  int A[100], B[100], sum[100];
  #pragma omp target map(to: A[0:50], B[0:50]) map(from: sum[0:50])
  #pragma omp parallel for
  for (int i = 0; i < 50; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

### Related resources

* [PWR015 examples at GitHub](/Checks/PWR015)

* [PWD005: Array range copied to the GPU does not cover the used range](/Checks/PWD005/README.md)
