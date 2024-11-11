# PWR051: Consider applying multithreading parallelism to scalar reduction loop

### Issue

A loop containing the
[scalar reduction](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)
pattern can be sped up using [multithreading](../../Glossary/Multithreading.md).

### Actions

Implement a version of the scalar reduction loop using an Application Program
Interface (API) that enables multithreading on the CPU. Codee assists the
programmer by providing source code rewriting capabilities using OpenMP compiler
directives.

### Relevance

Executing a loop using multithreading on the CPU is one of the ways to speed it
up. Multicore CPUs are widely used in modern computers, but writing
multithreaded code is not straightforward. Essentially, the programmer must
explicitly specify how to execute the loop in vector mode on the hardware, as
well as add the appropriate synchronization to avoid race conditions at runtime.
Typically, minimizing the computational overhead of multithreading is the
biggest challenge to speedup the code.

> [!NOTE]
> Executing scalar reduction loops using multithreading incurs an overhead due to
> the synchronization needed to avoid race conditions and ensure the correctness
> of the code. Note appropriate data scoping of shared and private variables is
> still a must.

### Code example

#### C

```c
double example(double *A, int n) {
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
directives for multithreading. Note the synchronization added to avoid race
conditions:

```c
double example(double *A, int n) {
  double sum = 0.0;

  #pragma omp parallel default(none) shared(A, n, sum) private(i)
  {
    #pragma omp for reduction(+: sum) schedule(auto)
    for (int i = 0; i < n; ++i) {
      sum += A[i];
    }
  } // end parallel

  return sum;
}
```

> [!NOTE]
> Executing scalar reduction loops using multithreading incurs a synchronization
> overhead. The example above shows a code that uses an efficient implementation
> balancing synchronization and memory overheads, by taking advantage of a
> reduction mechanism typically supported by the APIs for multithreading.

#### Fortran

```fortran
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

The code snippet below shows an implementation that uses the OpenMP compiler
directives for multithreading. Note the synchronization added to avoid race
conditions:

```fortran
function example(A) result(sum)
  implicit none
  real(kind=8), intent(in) :: A(:)
  real(kind=8) :: sum
  integer :: i

  sum = 0.0
  !$omp parallel do default(none) shared(A) private(i) reduction(+: sum) &
  !$omp& schedule(auto)
  do i = 1, size(A, 1)
    sum = sum + A(i)
  end do
end function example
```

> [!NOTE]
> Executing scalar reduction loops using multithreading incurs a synchronization
> overhead. The example above shows a code that uses an efficient implementation
> balancing synchronization and memory overheads, by taking advantage of a
> reduction mechanism typically supported by the APIs for multithreading.

### Related resources

* [PWR051 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR051/)

### References

* [Scalar reduction pattern](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)

* [Multithreading](../../Glossary/Multithreading.md)
