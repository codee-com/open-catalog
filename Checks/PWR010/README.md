# PWR010: Avoid column-major array access in C/C++

### Issue

In the  C and C++ programming languages, matrices are stored in a
[row-major layout](../../Glossary/Row-major-and-column-major-order.md); thus,
iterating the matrix column-wise is non-optimal and should be avoided if
possible.

### Actions

Change the code to access the multi-dimensional array in a row-wise manner.

### Relevance

The most efficient way to process arrays is to iterate over its elements in the
same order in which they are laid out in memory, so that the program performs a
sequential access to consecutive data in memory. The C and C++ language
specifications state that matrices are laid out in memory in a
[row-major order](../../Glossary/Row-major-and-column-major-order.md): the elements
of the first row are laid out consecutively in memory, followed by the elements
of the second row, and so on. As a result, in order to maximize performance, the
C and C++ code should access multi-dimensional arrays in a row-wise manner.

### Code example

In the following code, the matrix `A` is accessed in a column-wise manner. Given
a value of the loop iterator variable `j`, the `j`-th column of `A` is accessed
by increasing the loop iterator variable `i` of the innermost loop. As the
matrix `A` is stored according to a row-major layout in C/C++, this code is
accessing non-consecutive memory locations.

```c
for (int j = 0; j < n; ++j) {
  for (int i = 0; i < n; ++i) {
    A[i][j] = 0;
  }
}
```

The optimal way is to iterate over the memory sequentially. The code below
performs the same computations, the difference being that matrix `A` is accessed
in row-major order matching the row-major layout of C/C++. Thus, given a value
`i`, the `i`-th row of `A` is accessed by increasing the loop iterator variable
`j` of the innermost loop. Note that in the example above the loop nest order is
`ji` while now it is `ij`.

```c
for (int i = 0; i < n; ++i) {
  for (int j = 0; j < n; ++j) {
    A[i][j] = 0;
  }
}
```

### Related resources

* [PWR010 examples](../PWR010)

### References

* [Row-Major and Column-Major Array Layouts - MATLAB & Simulink](https://www.mathworks.com/help/coder/ug/what-are-column-major-and-row-major-representation-1.html)

* [Loop interchange](../../Glossary/Loop-interchange.md)
