# PWR056: Consider applying offloading parallelism to scalar reduction loop

### Issue

The loop containing the
[scalar reduction](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)
pattern can be sped up by
[offloading it to an accelerator](../../Glossary/Offloading.md).

### Actions

Implement a version of the scalar reduction loop using an Application Program
Interface (API) that enables offloading to accelerators. Codee assists the
programmer by providing source code rewriting capabilities using
[OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[OpenACC](https://en.wikipedia.org/wiki/OpenACC) compiler directives.

### Relevance

Offloading a loop to an accelerator is one of the ways to speed it up.
Accelerators offer a huge computational power, but writing code for accelerators
is not straightforward. Essentially, the programmer must explicitly manage the
data transfers between the host and the accelerator, specify how to execute the
loop in parallel on the accelerator, as well as add the appropriate
synchronization to avoid race conditions at runtime.

Typically, **minimizing the computational overhead of offloading is the biggest
challenge to speedup the code using accelerators**.

>**Note**  
>Offloading scalar reduction loops incurs an overhead due to the synchronization
>needed to avoid race conditions and ensure the correctness of the code. Note
>appropriate data scoping of shared and private variables is still a must.

### Code example

#### C

```c
double example(double *A, int n) {
  double sum = 0;

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

The code snippet below shows an implementation that uses the OpenACC compiler
directives to offload the loop to an accelerator. Note the synchronization
added to avoid race conditions, while the data transfer clauses manage the data
movement between the host memory and the accelerator memory:

```c
double example(double *A, int n) {
  double sum = 0;

  #pragma acc data copyin(A[0:n], n) copy(sum)
  #pragma acc parallel
  #pragma acc loop reduction(+: sum)
  for (int i = 0; i < n; ++i) {
    sum += A[i];
  }

  return sum;
}
```

#### Fortran

```f90
function example(A) result(sum)
  implicit none
  real(kind=8), intent(in) :: A(:)
  real(kind=8) :: sum
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

The code snippet below shows an implementation that uses the OpenACC compiler
directives to offload the loop to an accelerator. Note the synchronization
added to avoid race conditions, while the data transfer clauses manage the data
movement between the host memory and the accelerator memory:

```f90
function example(A) result(sum)
  implicit none
  real(kind=8), intent(in) :: A(:)
  real(kind=8) :: sum
  integer :: i

  sum = 0.0

  !$acc data copyin(A) copy(sum)
  !$acc parallel
  !$acc loop reduction(+: sum)
  do i = 1, size(A, 1)
    sum = sum + A(i)
  end do
  !$acc end parallel
  !$acc end data
end function example
```

### Related resources

* [PWR056 examples](../PWR056)

### References

* [Scalar reduction pattern](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)

* [Offloading](../../Glossary/Offloading.md)
