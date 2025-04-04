# PWR054: Consider applying vectorization to scalar reduction loop

### Issue

The loop containing a
[scalar reduction](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)
pattern can be sped up using [vectorization](../../Glossary/Vectorization.md).

### Actions

Implement a version of the scalar reduction loop using an Application Program
Interface (API) that enables vectorization, such as OpenMP, or using
compiler-specific directives.

### Relevance

Vectorizing a loop is one of the ways to speed it up. Vectorization is widely
used in modern computers, but writing vectorized code is not straightforward.
Essentially, the programmer must explicitly specify how to execute the loop in
vector mode on the hardware, as well as add the appropriate synchronization to
avoid race conditions at runtime. Typically, the compiler does a good job in
vectorization, so the biggest challenge is to vectorize loops manually beyond
the capabilities of the compiler.

> [!NOTE]
> Vectorizing scalar reduction loops incurs an overhead due to the
> synchronization needed to avoid race conditions and ensure the correctness of
> the code. Note appropriate data scoping of shared and private variables is
> still a must.

### Code example

#### C

```c
__attribute__((pure)) double example(double *A, int n) {
  double sum = 0.0;

  for (int i = 0; i < n; ++i) {
    sum += A[i];
  }

  return sum;
}
```

The loop body has a `scalar reduction` pattern, meaning that each iteration of
the loop *reduces* its computational result to a single value; in this case,
`sum`. Thus, any two iterations of the loop executing concurrently can
potentially update the value of the scalar `sum` at the same time. This creates
a potential race condition that must be handled through appropriate
synchronization.

The code snippet below shows an implementation that uses the OpenMP compiler
directives to explicitly vectorize the loop. Note the synchronization added to
avoid race conditions:

```c
__attribute__((pure)) double example(double *A, int n) {
  double sum = 0.0;

  #pragma omp simd reduction(+: sum)
  for (int i = 0; i < n; ++i) {
    sum += A[i];
  }

  return sum;
}
```

#### Fortran

```fortran
pure function example(A) result(sum)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:)
  real(kind=real32) :: sum
  integer :: i

  sum = 0.0
  do i = 1, size(A, 1)
    sum = sum + A(i)
  end do
end function example
```

The loop body has a `scalar reduction` pattern, meaning that each iteration of
the loop *reduces* its computational result to a single value; in this case,
`sum`. Thus, any two iterations of the loop executing concurrently can
potentially update the value of the scalar `sum` at the same time. This creates
a potential race condition that must be handled through appropriate
synchronization.

The code snippet below shows an implementation that uses the OpenMP compiler
directives to explicitly vectorize the loop. Note the synchronization added to
avoid race conditions:

```fortran
pure function example(A) result(sum)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:)
  real(kind=real32) :: sum
  integer :: i

  sum = 0.0
  !$omp simd reduction(+: sum)
  do i = 1, size(A, 1)
    sum = sum + A(i)
  end do
end function example
```

### Related resources

* [PWR054 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR054/)

### References

* [Scalar reduction pattern](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)

* [Vectorization](../../Glossary/Vectorization.md)
