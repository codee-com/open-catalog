# PWR062: Consider loop interchange by removing accumulation on array value

### Issue

The performance of the nested loops can benefit from [loop
interchange](../../Glossary/Loop-interchange.md), but an accumulation on the
array values prevents it.

### Actions

Perform [loop fission](../../Glossary/Loop-fission.md) to split the
accumulation into two parts: (1) a loop that initializes the accumulation, and
(2) a second loop that performs the actual computation of the accumulation.
This allows for a perfect nesting in the latter loop (performance critical),
which in turn enables loop interchange.

### Relevance

Inefficient [memory access patterns](../../Glossary/Memory-access-pattern.md)
and low [locality of reference](../../Glossary/Locality-of-reference.md) are
among the main reasons for low performance on modern computer systems. Matrices
are [stored in a row-major order in C/C++, and column-major order in
Fortran](../../Glossary/Row-major-and-column-major-order.md). Iterating over
them column-wise (in C) and row-wise (in Fortran) hinders performance as a
result of a suboptimal usage of the memory subsystem.

Nested loops that iterate over matrices inefficiently can be optimized by
applying loop interchange. This technique allows to replace the inefficient
memory access pattern with a more efficient one. Moreover, loop interchange
usually enables [vectorization](../../Glossary/Vectorization.md) of the
innermost loop as well, which can lead to additional performance improvements.

To perform the loop interchange, the loops need to be [perfectly
nested](../../Glossary/Perfect-loop-nesting.md); i.e., all the statements need
to be inside the innermost loop. However, in this scenario, the initialization
and computation of the accumulation is in separate statements, preventing the
loop interchange.

### Code example

#### C

Have a look at the following matrix multiplication code:

```c
void matmul(int n, const double *A, const double *B, double *C) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      C[i * n + j] = 0.0;
      for (int k = 0; k < n; ++k) {
        C[i * n + j] += A[i * n + k] * B[k * n + j];
      }
    }
  }
}
```

Note that, in the innermost loop, the memory access pattern of the matrix `B`
is strided. Hence, the computation can benefit from loop interchange for a more
efficient memory usage. However, the accumulation initialization `C[i * n + j]
= 0.0` prevents it.

To make the loop interchangeable, loop fission can be applied to move the
initialization to a separate loop:

```c
void matmul(int n, const double *A, const double *B, double *C) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      C[i * n + j] = 0.0;
    }
  }
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      for (int k = 0; k < n; ++k) {
        C[i * n + j] += A[i * n + k] * B[k * n + j];
      }
    }
  }
}
```

Now that the performance critical loop (the second one) is perfectly nested,
the loop interchange can be applied to leverage an efficient memory access
pattern:

```c
void matmul(int n, const double *A, const double *B, double *C) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      C[i * n + j] = 0.0;
    }
  }
  for (int i = 0; i < n; ++i) {
    for (int k = 0; k < n; ++k) {
      for (int j = 0; j < n; ++j) {
        C[i * n + j] += A[i * n + k] * B[k * n + j];
      }
    }
  }
}
```

#### Fortran

Have a look at the following matrix multiplication code:

```f90
subroutine matmul(n, A, B, C)
  implicit none
  integer, intent(in) :: n
  real, dimension(:, :), intent(in) :: A, B
  real, dimension(:, :), intent(out) :: C
  integer :: i, j, k

  do j = 1, n
    do i = 1, n
      C(i, j) = 0
      do k = 1, n
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
end subroutine matmul
```

Note that, in the innermost loop, the memory access pattern of the matrix `A`
is strided. Hence, the computation can benefit from loop interchange for a more
efficient memory usage. However, the accumulation initialization `C(i, j) = 0`
prevents it.

To make the loop interchangeable, loop fission can be applied to move the
initialization to a separate loop:

```f90
subroutine matmul(n, A, B, C)
  implicit none
  integer, intent(in) :: n
  real, dimension(:, :), intent(in) :: A, B
  real, dimension(:, :), intent(out) :: C
  integer :: i, j, k

  ! No need to write an explicit loop to zero the whole matrix
  C = 0

  do j = 1, n
    do i = 1, n
      do k = 1, n
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
end subroutine matmul
```

Now that the performance critical loop (the second one) is perfectly nested,
the loop interchange can be applied to leverage an efficient memory access
pattern:

```f90
subroutine matmul(n, A, B, C)
  implicit none
  integer, intent(in) :: n
  real, dimension(:, :), intent(in) :: A, B
  real, dimension(:, :), intent(out) :: C
  integer :: i, j, k

  C = 0

  do j = 1, n
    do k = 1, n
      do i = 1, n
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
end subroutine matmul
```

### Related resources

* [Source code examples and solutions](../PWR062/)

* [PWR010: Avoid column-major array access in C/C++](../PWR010/README.md)

* [PWR039: Consider loop interchange to improve the locality of reference and enable vectorization](../PWR039/README.md)

* [Loop fission](../../Glossary/Loop-fission.md)
