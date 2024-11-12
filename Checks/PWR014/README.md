# PWR014: Out-of-dimension-bounds matrix access

### Issue

An index outside the dimensions of a matrix is used.

### Actions

Change the array access to explicitly access the desired dimension.

### Relevance

C/C++ array access syntax allows the use of indices outside the ranges of array
elements. Using indices corresponding to positions outside the array is an out-
of-memory-bounds error. For matrices, however, using an index outside the
dimension bounds doesn't necessarily cause an out-of-memory-bound error, since
this access can result in accessing a valid array element in a different
dimension. However, it produces obscure code which is difficult to understand
and reason about. Moreover, parallelization of such code can lead to the
introduction of data races.

### Code example

The following code iterates over part of a two-dimensional array:

```c
void example() {
  int A[100][100];
  for (int i = 1; i < 100; ++i) {
    for (int j = 0; j < 100; ++j) {
      A[i][j - 1] = 1;
    }
  }
}
```

Notice that the second dimension is accessed by subtracting 1 from `j`. Since
the first value of `j` will be 0, the first iteration will access the second
dimension using an index of -1. This is valid syntax in C/C++: given that the
two-dimensional array is stored in contiguous memory, it results in accessing
the last element of the previous dimension. For instance, for the first
iteration of the loop, `A[1][-1]` will be accessed, which results in the same
element as accessing `A[0][99]`. Notice that if `i` range started in 0 instead
of 1, this would be an out-of-memory-bounds access defect; and the same would
happen if the two-dimensional array was dynamically allocated.

Although it may not be an error, this practice is discouraged since it makes it
harder to reason about the code. For example, the programmer might have wanted
to iterate over `j` up to 98, in which case the fixed code would be:

```c
void example() {
  int A[100][100];
  for (int i = 1; i < 100; ++i) {
    for (int j = 0; j < 99; ++j) {
      A[i][j] = 1;
    }
  }
}
```

### Related resources

* [PWR014 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR014/)

### References

* [Index checking - Bounds checking](https://en.wikipedia.org/wiki/Bounds_checking#Index_checking)
[last checked October 2020]
