# PWD011: Missing OpenMP lastprivate clause

### Issue

A variable is being incorrectly privatized in the OpenMP datascoping clauses.

### Actions

Change the data scope of the variable to `lastprivate`.

### Relevance

In some parallel loops, variables need to be private to each thread but the
value computed in the last loop iterations needs to be used afterwards. For such
cases, the OpenMP `private` scoping is incorrect and `lastprivate` must be used
instead.

### Code example

#### C

In the following example, `liveOut` is incorrectly marked as `private`. When a
variable is marked as `private`, each thread is instructed to make its own copy
and the original variable is not updated during parallel execution. As a
result, once the loop is completed, the original `liveOut` won't retain the
value from the last iteration:

```c
double example(int m, double *A, double *B, double *C) {
  double liveOut;

  // liveOut is private but used after the loop, so it should be lastprivate
  #pragma omp parallel for private(liveOut) shared(A, B, C)
  for (int i = 0; i < m; i++) {
    liveOut = A[i] * B[i];
    C[i] = C[i] + liveOut;
  }

  liveOut += 5;
  return liveOut;
}
```

To achieve this behavior, we can use the `lastprivate` clause. This allows
subsequent operations on `liveOut` to work correctly:

```c
double example(int m, double *A, double *B, double *C) {
  double liveOut;

  #pragma omp parallel for lastprivate(liveOut) shared(A, B, C)
  for (int i = 0; i < m; i++) {
    liveOut = A[i] * B[i];
    C[i] = C[i] + liveOut;
  }

  liveOut += 5;
  return liveOut;
}
```

#### Fortran

In the following example, `liveOut` is incorrectly marked as `private`. When a
variable is marked as `private`, each thread is instructed to make its own copy
and the original variable is not updated during parallel execution. As a
result, once the loop is completed, the original `liveOut` won't retain the
value from the last iteration:

```fortran
real function example(A, B, C)
  implicit none
  real, intent(in) :: A(:), B(:)
  real, intent(inout) :: C(:)
  real :: liveOut
  integer :: i

  !$omp parallel do private(i, liveOut) shared(A, B, C)
  do i = 1, size(C, 1)
    liveOut = A(i) * B(i)
    C(i) = C(i) + liveOut
  end do

  liveOut = liveOut + 5
  example = liveOut
end function example
```

To achieve this behavior, we can use the `lastprivate` clause. This allows
subsequent operations on `liveOut` to work correctly:

```fortran
real function example(A, B, C)
  implicit none
  real, intent(in) :: A(:), B(:)
  real, intent(inout) :: C(:)
  real :: liveOut
  integer :: i

  !$omp parallel do lastprivate(liveOut) private(i) shared(A, B, C)
  do i = 1, size(C, 1)
    liveOut = A(i) * B(i)
    C(i) = C(i) + liveOut
  end do

  liveOut = liveOut + 5
  example = liveOut
end function example
```

### Related resources

* [PWD011 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWD011/)

### References

* [Data-Sharing Attribute Clauses - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu106.html)
[last checked August 2021]
