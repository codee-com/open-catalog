# PWR052: Consider applying multithreading parallelism to sparse reduction loop

### Issue

A loop containing the
[sparse reduction](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md)
pattern can be sped up using [multithreading](../../Glossary/Multithreading.md).

### Actions

Implement a version of the sparse reduction loop using an Application Program
Interface (API) that enables multithreading on the CPU. Codee assists the
programmer by providing source code rewriting capabilities using OpenMP compiler
directives.

### Relevance

Executing a loop using [multithreading](../../Glossary/Multithreading.md) on the CPU
is one of the ways to speed it up. Multicore CPUs are widely used in modern
computers, but writing multithreaded code is not straightforward. Essentially,
the programmer must explicitly specify how to execute the loop in vector mode on
the hardware, as well as add the appropriate synchronization to avoid race
conditions at runtime. Typically, minimizing the computational overhead of
multithreading is the biggest challenge to speedup the code.

>**Note**  
>Executing sparse reduction loops using multithreading incurs an overhead due to
>the synchronization needed to avoid race conditions and ensure the correctness
>of the code. Note appropriate data scoping of shared and private variables is
>still a must.

### Code example

#### C

```c
void example(double *A, int *nodes, int n) {
  for (int nel = 0; nel < n; ++nel) {
    A[nodes[nel]] += nel * 1;
  }
}
```

The loop body has a `sparse reduction` pattern, meaning that each iteration of
the loop *reduces* its computational result to a value, but the place where the
value is stored is known at runtime only. Thus, any two iterations of the loop
executing concurrently can potentially update the same element of the array `A`
at the same time. This creates a potential race condition that must be handled
through appropriate synchronization.

The code snippet below shows an implementation that uses the OpenMP compiler
directives for multithreading. Note the synchronization added to avoid race
conditions:

```c
void example(double *A, int *nodes, int n) {
  #pragma omp parallel default(none) shared(A, n, nodes) private(nel)
  {
    #pragma omp for schedule(auto)
    for (int nel = 0; nel < n; ++nel) {
      #pragma omp atomic update
      A[nodes[nel]] += nel * 1;
    }
  } // end parallel
}
```

>**Note**  
>Executing sparse reduction loops using multithreading incurs a synchronization
>overhead. The example above shows an implementation that uses atomic
>protection. Other implementations reduce this high overhead taking advantage of
>privatization, which increases the memory requirements of the code. An
>efficient implementation that balances synchronization and memory overheads
>must be explored for each particular code.

#### Fortran

```f90
subroutine example(A, nodes)
  implicit none
  real(kind=8), intent(inout) :: A(:)
  integer, intent(in) :: nodes(:)
  integer :: nel

  do nel = 1, size(nodes, 1)
    A(nodes(nel)) = A(nodes(nel)) + (nel * 1)
  end do
end subroutine example
```

The loop body has a `sparse reduction` pattern, meaning that each iteration of
the loop *reduces* its computational result to a value, but the place where the
value is stored is known at runtime only. Thus, any two iterations of the loop
executing concurrently can potentially update the same element of the array `A`
at the same time. This creates a potential race condition that must be handled
through appropriate synchronization.

The code snippet below shows an implementation that uses the OpenMP compiler
directives for multithreading. Note the synchronization added to avoid race
conditions:

```f90
subroutine example(A, nodes)
  implicit none
  real(kind=8), intent(inout) :: A(:)
  integer, intent(in) :: nodes(:)
  integer :: nel

  !$omp parallel do default(none) shared(A, nodes) private(nel) schedule(auto)
  do nel = 1, size(nodes, 1)
    !$omp atomic update
    A(nodes(nel)) = A(nodes(nel)) + (nel * 1)
  end do
end subroutine example
```

>**Note**  
>Executing sparse reduction loops using multithreading incurs a synchronization
>overhead. The example above shows an implementation that uses atomic
>protection. Other implementations reduce this high overhead taking advantage
>of privatization, which increases the memory requirements of the code. An
>efficient implementation that balances synchronization and memory overheads
>must be explored for each particular code.

### Related resources

* [PWR052 examples](../PWR052/)

### References

* [Sparse reduction pattern](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md)

* [Multithreading](../../Glossary/Multithreading.md)
