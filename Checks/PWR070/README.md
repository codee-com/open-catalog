# PWR070: Declare array dummy arguments as assumed-shape arrays

### Issue

When procedures receive arrays in explicit-shape or assumed-size form,
programmers are required to manually pass array properties as additional
arguments. This practice limits the compiler's ability to perform compatibility
checks, increasing the risk of difficult-to-diagnose runtime bugs.
Additionally, these types of arrays can result in suboptimal performance
compared to assumed-shape arrays.

### Actions

To improve both code safety and performance, transform explicit-shape and
assumed-size array dummy arguments to assumed-shape form.

### Relevance

Fortran supports various methods for passing allocated arrays to procedures.
Typically, assumed-shape arrays should be preferred over explicit-shape and
assumed-size arrays, as they provide equivalent functionality while being safer
and more efficient:

- **Assumed-shape arrays** are simply declared with a colon for each dimension;
    e.g., `real :: arr(:)`, `real :: arr(:, :)`. They offer several benefits:
  - Compile-time checks for the compatibility of the passed array's rank.
  - Automatic deduction of the size of each dimension from the passed array,
      accessible via `shape(arr)` and `size(arr, dim)`.

- **Explicit-shape and assumed-size arrays** require manual specification of
  dimension sizes as separate arguments, increasing the likelihood of errors:
  - Explicit-shape arrays specify the size of all dimensions; e.g., `real ::
    arr(i, j)`.
  - Assumed-size arrays leave the size of the last dimension unspecified; e.g.,
    `real :: arr(n, *)`.
  - In general, they lack compile-time checks for consistency between the
    provided and the expected array.

Aditionally, explicit-shape and assumed-size dummy arguments require contiguous
memory. This forces the creation of intermediate data copies when working with
array slices or strided accesses. In contrast, assumed-shape arrays can handle
these scenarios directly, leading to enhanced performance.

### Code examples

Below are demonstrated the risks of assumed-size and explicit-shape arrays,
along with the benefits of assumed-shape arrays, using a program that
calculates the sum of each row in a matrix.

Let's start with the assumed-size example:

```fortran
! example-assumed-size.f90
program test_assumed_size
  implicit none
  integer, parameter :: rows = 2, cols = 3
  ! Each row contains "1, 2, 3"
  real :: matrix(rows, cols) = reshape([1.0, 2.0, 3.0, 1.0, 2.0, 3.0], [rows, cols])

  ! Should print "6" (1 + 2 + 3) for each row
  call sum_rows_assumed_size(matrix, cols, rows)

contains

subroutine sum_rows_assumed_size(arr, m, n)
  real, intent(in) :: arr(m, *)
  integer, intent(in) :: m, n
  integer :: i, j
  real :: sum

  do i = 1, m
    sum = 0.0
    do j = 1, n
      sum = sum + arr(i, j)
    end do
    print *, 'Row', i, 'Sum:', sum
  end do
end subroutine sum_rows_assumed_size
end program test_assumed_size
```

Did you notice that the dimensions of the matrix are incorrectly swapped in the
subroutine call? Unfortunately, the compiler cannot detect that the shape of
the passed array `(2, 3)` and the expected array `(3, 2)` don't match. The
contents of the memory are simply interpreted incorrectly, leading to the
following result:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example-assumed-size.f90
$ ./a.out
 Row           1 Sum:   2.00000000
 Row           2 Sum:   4.00000000
 Row           3 Sum:   6.00000000
```

The same error goes undetected by the compiler when using an explicit-shape
array:

```fortran
! example-explicit-shape.f90
program test_explicit_shape
  ...

  ! Should print "6" (1 + 2 + 3) for each row
  call sum_rows_explicit_shape(matrix, cols, rows)

contains

subroutine sum_rows_explicit_shape(arr, m, n)
  real, intent(in) :: arr(m, n)
  integer, intent(in) :: m, n
  integer :: i, j
  real :: sum

  do i = 1, m
    sum = 0.0
    do j = 1, n
      sum = sum + arr(i, j)
    end do
    print *, 'Row', i, 'Sum:', sum
  end do
end subroutine sum_rows_explicit_shape
end program test_explicit_shape
```

```txt
$ gfortran example-explicit-shape.f90
$ ./a.out
 Row           1 Sum:   2.00000000
 Row           2 Sum:   4.00000000
 Row           3 Sum:   6.00000000
```

As the assumed-shape automatically inherits the size of each dimension from the
call, these errors are prevented altogether:

```fortran
! solution.f90
program test_assumed_shape
  ...

  ! Should print "6" (1 + 2 + 3) for each row
  call sum_rows_assumed_shape(matrix)

contains

subroutine sum_rows_assumed_shape(arr)
  real, intent(in) :: arr(:, :)
  integer :: i, j
  real :: sum

  do i = 1, size(arr, 1)
    sum = 0.0
    do j = 1, size(arr, 2)
      sum = sum + arr(i, j)
    end do
    print *, 'Row', i, 'Sum:', sum
  end do
end subroutine sum_rows_assumed_shape
end program test_assumed_shape
```

And the program gives the correct result:

```txt
$ gfortran solution.f90
$ ./a.out
 Row           1 Sum:   6.00000000
 Row           2 Sum:   6.00000000
```

> [!TIP]
> As explained previously, if the subroutines operate on a slice of the matrix,
> assumed-shape arrays can manage the slice directly, potentially improving
> performance.
>
> Check the PWR070 benchmark for a demonstration!

> [!WARNING]
> Beware that any procedures involving assumed-shape array arguments must have
> explicit interfaces at the point of call. If not, the updated code won't
> compile.
>
> Check the [PWR068 entry](../PWR068/) for more details on implicit and explicit
> interfaces!

### Related resources

- [PWR070 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR070/)

### References

- ["Arrays -- Fortran Programming
Language"](https://fortran-lang.org/en/learn/best_practices/arrays/), Fortran
Community. [last checked May 2024]

- ["Modernizing Old Fortran in Fortran
Wiki"](https://fortranwiki.org/fortran/show/Modernizing+Old+Fortran), Fortran
Community. [last checked May 2024]

- ["Explicit-shape and assumed-size arrays, and sequence
association"](https://fortran-lang.discourse.group/t/explicit-shape-and-assumed-size-arrays-and-sequence-association/2783),
Fortran Community. [last checked May 2024]

- ["Difference between assumed-size and assumed-shape arrays? -- Fortran
Discourse"](https://fortran-lang.discourse.group/t/difference-between-assumed-size-and-assumed-shape-arrays/6923/1),
Fortran Community. [last checked May 2024]

- ["Passing arrays to subroutines in Fortran: Assumed shape vs explicit
shape"](https://stackoverflow.com/questions/75051887/passing-arrays-to-subroutines-in-fortran-assumed-shape-vs-explicit-shape),
Stack Overflow Community. [last checked May 2024]

- ["Fortran Modernisation
Workshop"](https://blog.rwth-aachen.de/hpc_import_20210107/attachments/39157901/39420371.pdf),
The Numerical Algorithms Group. [last checked May 2024]

- ["Assumed shape array discussion in Fortran
Wiki"](https://fortranwiki.org/fortran/show/Assumed+shape+array+discussion),
Fortran Community. [last checked May 2024]
