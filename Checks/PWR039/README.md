# PWR039: Consider loop interchange to improve the locality of reference and enable vectorization

### Issue

Inefficient
[matrix access pattern](../../Glossary/Row-major-and-column-major-order.md) detected
that can be improved through [loop interchange](../../Glossary/Loop-interchange.md).

### Actions

Interchange the inner and outer loops in the loop nest.

### Relevance

Inefficient [memory access pattern](../../Glossary/Memory-access-pattern.md) and low
[locality of reference](../../Glossary/Locality-of-reference.md) are among the main
reasons for low performance on modern computer systems. Matrices are
[stored in a row-major order in C and column-major order in Fortran](../../Glossary/Row-major-and-column-major-order.md).
Iterating over them column-wise (in C) and row-wise (in Fortran) is inefficient,
because it uses the memory subsystem suboptimally.

Nested loops that iterate over matrices inefficiently can be optimized by
applying loop interchange. Using loop interchange, the inefficient matrix access
pattern is replaced with a more efficient one. Often, loop interchange enables
[vectorization](../../Glossary/Vectorization.md) of the innermost loop which
additionally improves performance.

>**Note**  
>Loop interchange can be performed only on perfectly nested loops, i.e. on loops
>where all the statements are in the body of the innermost loop. If the loops
>are not perfectly nested, it is often possible to make them
>[perfectly nested through refactoring](../../Glossary/Perfect-loop-nesting.md).

### Code example

#### C

The following code shows two nested loops:

```c
void example(double **A, int n) {
  for (int j = 0; j < n; j++) {
    for (int i = 0; i < n; i++) {
      A[i][j] = 0.0;
    }
  }
}
```

The matrix `A` is accessed column-wise, which is inefficient since C stores
arrays using row-major order. To fix this issue, a loop interchange can be
applied on loops `i` and `j`. As a result, the loop over `i` becomes the outer
loop, and the loop over `j` becomes the inner one:

```c
void example(double **A, int n) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      A[i][j] = 0.0;
    }
  }
}
```

After this modification, the access to matrix `A` is no longer column-wise, but
row-wise, resulting in a more efficient usage of the memory subsystem, and
thus, faster execution. Additionally, this optimization can help the compiler
vectorize the inner loop.

#### Fortran

The following code shows two nested loops:

```f90
subroutine example(A)
  real, intent(out) :: A(:, :)
  integer :: i, j

  do i = 1, size(A, 1)
    do j = 1, size(A, 2)
      A(i, j) = 0.0
    end do
  end do
end subroutine example
```

The matrix `A` is accessed row-wise, which is inefficient since Fortran stores
arrays using column-major order. To fix this issue, a loop interchange can be
applied on loops `i` and `j`. As a result, the loop over `j` becomes the outer
loop, and the loop over `i` becomes the inner one:

```f90
subroutine example(A)
  real, intent(out) :: A(:, :)
  integer :: i, j

  do j = 1, size(A, 2)
    do i = 1, size(A, 1)
      A(i, j) = 0.0
    end do
  end do
end subroutine example
```

After this modification, the access to matrix `A` is no longer row-wise, but
column-wise, resulting in a more efficient usage of the memory subsystem, and
thus, faster execution. Additionally, this optimization can help the compiler
vectorize the inner loop.

### Related resources

* [Source code examples and solutions](../PWR039)
