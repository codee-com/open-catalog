# PWD008: Unprotected multithreading recurrence due to out-of-dimension-bounds array access

### Issue

An index outside the dimension size of an array is being used to access it,
resulting in an access to a different dimension which introduces a data race in
the form of an unprotected recurrence.

### Actions

Explicitly access each array dimension and ensure that the recurrence is
executed correctly in parallel.

### Relevance

The array access syntax in C, C++ and Fortran allows using indices that fall
outside the defined dimensions of the array.

It's technically possible to access valid positions in different dimensions by
specifying indices beyond the bounds of the array. For instance, in C, the
element before the start of a row corresponds to the last element of the
previous row, since data is stored in row-major order. In contrast, Fortran
stores data in column-major order.

In any case, code that uses indices exceeding the array bounds is obscure and
hard to understand, and can also easily lead to runtime errors. Additionally,
in parallel programming, this practice can also create data races when there is
a recurrence pattern in the computation.

### Code example

#### C

The following code iterates over the rows of a 2D array in parallel, with each
thread processing sequentially the columns of its rows. However, there is
an issue with out-of-dimension-bounds access that causes a thread to access an
element from a different row. This leads to a data race between the threads:

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

Assuming that the original intention is for each thread to process a row, the
issue can be resolved by starting the column iterations from index `1` instead
of `0`:

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

#### Fortran

The following code iterates over the columns of a 2D array in parallel, with
each thread processing sequentially the rows of its columns. However, there is
an issue with out-of-dimension-bounds access that causes a thread to access an
element from a different column. This leads to a data race between the threads:

```f90
subroutine example(A)
  integer, intent(inout) :: A(:, :)
  integer :: i, j

  !$omp parallel do
  do j = 2, size(A, 2)
    do i = 1, size(A, 1)
      A(i, j) = A(i, j) + A(i - 1, j)
    end do
  end do
end subroutine example
```

Assuming that the original intention is for each thread to process a column,
the issue can be resolved by starting the row iterations from index `2`
instead of `1`:

```f90
subroutine example(A)
  integer, intent(inout) :: A(:, :)
  integer :: i, j

  !$omp parallel do
  do j = 2, size(A, 2)
    do i = 2, size(A, 1)
      A(i, j) = A(i, j) + A(i - 1, j)
    end do
  end do
end subroutine example
```

### Related resources

* [PWD008 examples](../PWD008)

* [PWD007: Unprotected multithreading recurrence](../PWD007/README.md)

* [PWR014: Out-of-dimension-bounds array access](../PWR014/README.md)

### References

* [Recurrence pattern](../../Glossary/Patterns-for-performance-optimization/Recurrence.md)

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
