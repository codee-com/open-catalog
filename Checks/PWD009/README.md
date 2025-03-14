# PWD009: Incorrect privatization in OpenMP parallel region

### Issue

A variable is being incorrectly privatized in the OpenMP datascoping clauses.

### Actions

Change the data scope of the variable from private to shared.

### Relevance

Specifying an invalid scope for a variable may introduce race conditions and
produce incorrect results. For instance, when a variable must be shared among
threads but it is privatized instead.

### Code example

#### C

In the following code, `C` is incorrectly privatized since the threads operate
on non-overlapping positions of the array:

```c
void example(int m, double *A, double *B, double *C) {
  #pragma omp parallel for default(none) private(C) shared(A, B, m)
  for (int i = 0; i < m; i++) {
    double temp = A[i] * B[i];
    C[i] = C[i] + temp;
  }
}
```

To fix this, `C` should be moved to a `shared` clause:

```c
void example(int m, double *A, double *B, double *C) {
  #pragma omp parallel for default(none) shared(A, B, C, m)
  for (int i = 0; i < m; i++) {
    double temp = A[i] * B[i];
    C[i] = C[i] + temp;
  }
}
```

#### Fortran

In the following code, `C` is incorrectly privatized since the threads operate
on non-overlapping positions of the array:

```fortran
subroutine example(A, B, C)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:), B(:)
  real(kind=real32), intent(inout) :: C(:)
  real(kind=real32) :: temp
  integer :: i

  !$omp parallel do default(none) private(i, temp, C) shared(A, B)
  do i = 1, size(C, 1)
    temp = A(i) * B(i)
    C(i) = C(i) + temp
  end do
end subroutine example
```

To fix this, `C` should be moved to a `shared` clause:

```fortran
subroutine example(A, B, C)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:), B(:)
  real(kind=real32), intent(inout) :: C(:)
  real(kind=real32) :: temp
  integer :: i

  !$omp parallel do default(none) private(i, temp) shared(A, B, C)
  do i = 1, size(C, 1)
    temp = A(i) * B(i)
    C(i) = C(i) + temp
  end do
end subroutine example
```

### Related resources

* [PWD009 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWD009/)

### References

* [Data-Sharing Attribute Clauses - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu106.html)
[last checked August 2021]
