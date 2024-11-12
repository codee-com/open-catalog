# PWR013: Avoid copying unused variables to or from the GPU

### Issue

Unused variables should never be copied to or from the GPU to prevent
unnecessary [data movements](../../Glossary/Offloading.md) between the CPU and the
GPU, which impacts performance.

### Actions

Remove the unused variables from the data mapping clauses.

### Relevance

One of the key challenges when offloading work to the GPU is minimizing the data
transfers between CPU memory and GPU memory. These transfers can greatly affect
performance and should be minimized. Thus, only the strictly required data
should be copied to or from the GPU memory.

### Code example

#### C

In the following example, matrix `B` is copied to the GPU even when it is not
used:

```c
void example(double *A, double *B, double *C) {
  #pragma omp target teams distribute parallel for schedule(auto) shared(A, B) \
          private(i) map(to: A[0:100], B[0:100]) map(tofrom: C[0:100])
  for (int i = 0; i < 100; i++) {
    C[i] += A[i];
  }
}
```

This can be easily corrected by removing references to B from all the clauses:

```c
void example(double *A, double *B, double *C) {
  #pragma omp target teams distribute parallel for schedule(auto) shared(A) \
          private(i) map(to: A[0:100]) map(tofrom: C[0:100])
  for (int i = 0; i < 100; i++) {
    C[i] += A[i];
  }
}
```

#### Fortran

In the following example, matrix `B` is copied to the GPU even when it is not
used:

```f90
subroutine example(A, B, C)
  implicit none
  integer, intent(in) :: A(:), B(:)
  integer, intent(inout) :: C(:)
  integer :: i

  !$omp target teams distribute parallel do schedule(auto) default(none) &
  !$omp& shared(A, B, C) private(i) map(to: A, B) map(tofrom: C)
  do i = 1, size(C, 1)
    C(i) = C(i) + A(i)
  end do
  !$omp end target teams distribute parallel do
end subroutine example
```

This can be easily corrected by removing references to B from all the clauses:

```f90
subroutine example(A, B, C)
  implicit none
  integer, intent(in) :: A(:), B(:)
  integer, intent(inout) :: C(:)
  integer :: i

  !$omp target teams distribute parallel do schedule(auto) default(none) &
  !$omp& shared(A, C) private(i) map(to: A) map(tofrom: C)
  do i = 1, size(C, 1)
    C(i) = C(i) + A(i)
  end do
  !$omp end target teams distribute parallel do
end subroutine example
```

### Related resources

* [PWR013 examples](../PWR013/)
