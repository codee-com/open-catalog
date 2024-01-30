# PWD011: Missing OpenMP lastprivate clause

### Issue

A variable is being incorrectly privatized in the OpenMP datascoping clauses.

### Actions

Change the data scope of the variable to `lastprivate`.

### Relevance

In some parallel loops, variables need to be private to each thread but the
value computed in the last loop iterations needs to be used afterwards. For such
cases, the OpenMP `private` scoping is incorrect and `lastprivate` must be used
instead.

### Code example

In the following example, `liveOut` has an incorrect `private` clause:

```c
double example(int m, double *A, double *B, double *C) {
  double liveOut;

  // liveOut is private but used after the loop, should be lastprivate
  #pragma omp parallel for private(liveOut)
  for (int i = 0; i < m; i++) {
    liveOut = A[i] * B[i];
    C[i] = C[i] + liveOut;
  }

  liveOut += 5;
  return liveOut;
}
```

The clause needs to be updated to `lastprivate` so that the value computed in
the last loop iteration survives the loop:

```c
double example(int m, double *A, double *B, double *C) {
  double liveOut;

  #pragma omp parallel for lastprivate(liveOut)
  for (int i = 0; i < m; i++) {
    liveOut = A[i] * B[i];
    C[i] = C[i] + liveOut;
  }

  liveOut += 5;
  return liveOut;
}
```

### Related resources

* [PWD011 examples at GitHub](/Checks/PWD011)

### References

* [Data-Sharing Attribute Clauses - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu106.html)
[last checked August 2021]
