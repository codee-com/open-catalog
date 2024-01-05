# Row-major pattern

A bi-dimensional array memory access follows a row-major pattern when iterating
over the rows sequentially, then doing the same for each column within the row.

```c
for (int i = 0; i < N; i++) {
  for (int j = 0; j < N; j++) {
    A[i][j] = ...
  }
}
```

Note in the pseudocode that the outer loop drives the first dimension of the
array while the inner loop drives the second dimension.
