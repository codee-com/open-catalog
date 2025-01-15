# PWR055: Consider applying offloading parallelism to forall loop

### Issue

The loop containing the
[forall pattern](../../Glossary/Patterns-for-performance-optimization/Forall.md)
can be sped up by [offloading it to an accelerator](../../Glossary/Offloading.md).

### Actions

Implement a version of the forall loop using an Application Program Interface
(API) that enables offloading to accelerators, such as OpenMP or OpenACC.

### Relevance

Offloading a loop to an accelerator is one of the ways to speed it up.
Accelerators offer a huge computational power, but writing code for accelerators
is not straightforward. Essentially, the programmer must explicitly manage the
data transfers between the host and the accelerator, specify how to execute the
loop in parallel on the accelerator, as well as add the appropriate
synchronization to avoid race conditions at runtime. Typically, minimizing the
computational overhead of offloading is the biggest challenge to speedup the
code using accelerators.

> [!NOTE]
> Offloading forall loops typically incurs less overhead than offloading scalar
> reduction loops and sparse reduction loops. The main reason is that no
> synchronization is needed to avoid race conditions and ensure the correctness
> of the code. Note appropriate data scoping of shared and private variables is
> still a must.

### Code example

#### C

```c
void example(double *D, double *X, double *Y, int n, double a) {
  for (int i = 0; i < n; ++i) {
    D[i] = a * X[i] + Y[i];
  }
}
```

The loop body has a `forall` pattern, meaning that each iteration of the loop
can be executed independently and the result in each iteration is written to an
independent memory location. Thus, no race conditions can appear at runtime
related to array `D`, so no specific synchronization is needed.

The code snippet below shows an implementation that uses the OpenACC compiler
directives to offload the loop to an accelerator. Note how no synchronization
is required to avoid race conditions, while the data transfer clauses manage
the data movement between the host memory and the accelerator memory:

```c
void example(double *D, double *X, double *Y, int n, double a) {
  #pragma acc data copyin(X[0:n], Y[0:n], a, n) copyout(D[0:n])
  #pragma acc parallel
  #pragma acc loop
  for (int i = 0; i < n; ++i) {
    D[i] = a * X[i] + Y[i];
  }
}
```

#### Fortran

```fortran
subroutine example(D, X, Y, a)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(out) :: D(:)
  real(kind=real32), intent(in) :: X(:), Y(:)
  real(kind=real32), intent(in) :: a
  integer :: i

  do i = 1, size(D, 1)
    D(i) = a * X(i) + Y(i)
  end do
end subroutine example
```

The loop body has a `forall` pattern, meaning that each iteration of the loop
can be executed independently and the result in each iteration is written to an
independent memory location. Thus, no race conditions can appear at runtime
related to array `D`, so no specific synchronization is needed.

The code snippet below shows an implementation that uses the OpenACC compiler
directives to offload the loop to an accelerator. Note how no synchronization
is required to avoid race conditions, while the data transfer clauses manage
the data movement between the host memory and the accelerator memory:

```fortran
subroutine example(D, X, Y, a)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(out) :: D(:)
  real(kind=real32), intent(in) :: X(:), Y(:)
  real(kind=real32), intent(in) :: a
  integer :: i

  !$acc data copyin(X, Y, a) copyout(D)
  !$acc parallel
  !$acc loop
  do i = 1, size(D, 1)
    D(i) = a * X(i) + Y(i)
  end do
  !$acc end parallel
  !$acc end data
end subroutine example
```

>[!WARNING]
> OpenACC/OpenMP offloading directives are not allowed in Fortran procedures
> marked with the `pure` attribute. To enable their use, `pure` must be
> removed.

### Related resources

* [PWR055 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR055/)

### References

* [Forall pattern](../../Glossary/Patterns-for-performance-optimization/Forall.md)

* [Offloading](../../Glossary/Offloading.md)
