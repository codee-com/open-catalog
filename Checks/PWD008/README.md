# PWD008: Unprotected multithreading recurrence due to out-of-dimension-bounds array access

### Issue

An index outside the dimension size of an array is being used to access it,
resulting in an access to a different dimension which introduces a data race in
the form of an unprotected recurrence.

### Actions

Explicitly access each array dimension and ensure that the recurrence is
executed correctly in parallel.

### Relevance

C/C++ array access syntax allows the use of indices outside the ranges array
dimensions of even the array itself. Using indices corresponding to positions
outside the array constitutes an out-of-memory-bounds error. Using an index
outside the dimension bounds that results in accessing a valid array position in
a different dimension is technically correct; however, it produces obscure code
which is difficult to understand and reason about. Moreover, doing this in
parallel code can introduce a data race when there is a recurrence pattern.

### Code example

The following code iterates over the rows in parallel and each thread iterates
sequentially over the columns. However, there is an out-of-dimension-bounds
access causing that a thread access an array element of a different row, which
results in a data race between the corresponding threads:

```c
void foo() {
  int A[5][5];

  #pragma omp parallel for
  for (int i = 1; i < 5; ++i) {
    for (int j = 0; j < 5; ++j) {
      A[i][j] += A[i][j - 1];
    }
  }
}
```

Assuming that each thread is responsible for processing a row, this can be fixed
by starting the iterations over the second dimension in 1:

```c
void foo() {
  int A[5][5];

  #pragma omp parallel for
  for (int i = 1; i < 5; ++i) {
    for (int j = 1; j < 5; ++j) {
      A[i][j] += A[i][j-1];
    }
  }
}
```

### Related resources

* [PWD008 examples](../PWD008)

* [PWD007: Unprotected multithreading recurrence](../PWD007/README.md)

* [PWR014: Out-of-dimension-bounds array access](../PWR014/README.md)

### References

* [Recurrence pattern](../../Glossary/Patterns-for-performance-optimization/Recurrence.md)

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
