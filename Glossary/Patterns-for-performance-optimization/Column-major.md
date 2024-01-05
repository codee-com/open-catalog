# Column-major pattern

A bi-dimensional array memory access follows a column-major pattern when
iterating over the columns sequentially, then doing the same for each row within
the column.

```c
for (int j = 0; j < N; j++) {
  for (int i = 0; i < N; i++) {
    A[i][j] = ...
  }
}
```

Note in the pseudocode that the outer loop drives the second dimension of the
array while the inner loop drives the first dimension.
