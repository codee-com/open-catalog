# PWD009: Incorrect privatization in OpenMP parallel region

### Issue

A variable is being incorrectly privatized in the OpenMP datascoping clauses.

### Actions

Change the data scope of the variable from private to shared.

### Relevance

Specifying an invalid scope for a variable may introduce race conditions and
produce incorrect results. For instance, when a variable must be shared among
threads but it is privatized instead.

### Code example

In the following code, `C` is incorrectly privatized:

```c
void example(int m, double *A, double *B, double *C) {
  double temp;

  #pragma omp parallel for default(none) private(temp, C) shared(A, B, m)
  for (int i = 0; i < m; i++) {
    temp = A[i] * B[i];
    C[i] = C[i] + temp;
  }
}
```

To fix this, it should be moved to the `shared` clause:

```c
void example(int m, double *A, double *B, double *C) {
  double temp;

  #pragma omp parallel for default(none) private(temp) shared(A, B, C, m)
  for (int i = 0; i < m; i++) {
    temp = A[i] * B[i];
    C[i] = C[i] + temp;
  }
}
```

### Related resources

* [PWD009 examples at GitHub](/Checks/PWD009)

### References

* [Data-Sharing Attribute Clauses - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu106.html)
[last checked August 2021]
