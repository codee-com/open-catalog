# PWD010: Incorrect sharing in OpenMP parallel region

### Issue

A variable is being incorrectly shared in the OpenMP datascoping clauses.

### Relevance

Specifying an invalid scope for a variable may introduce race conditions and
produce incorrect results. For instance, when a variable is written from
parallel threads and the specified scoping is shared instead of private.

### Actions

Change the data scope of the variable from shared to private.

### Code example

In the following code no variable is privatized:

```c
void example(int **result, unsigned rows, unsigned cols) {
  int i, j;

  // j is implicitly shared and it should be private!
  #pragma omp parallel for shared(result)
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      result[i][j] = 0;
    }
  }
}
```

This introduced a data race due to `j` being shared among threads. It should be
privatized:

```c
void example(int **result, unsigned rows, unsigned cols) {
  int i, j;

  // j is implicitly shared and it should be private!
  #pragma omp parallel for shared(result) private(j)
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      result[i][j] = 0;
    }
  }
}
```

### Related resources

* [PWD010 examples at GitHub](/Checks/PWD010)

### References

* [Data-Sharing Attribute Clauses - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu106.html)
[last checked August 2021]
