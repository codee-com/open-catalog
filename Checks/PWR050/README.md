# PWR050: Consider applying multithreading parallelism to forall loop

### Issue

A loop containing the
[forall](/Glossary/Patterns-for-performance-optimization/Forall.md) pattern can
be sped up using [multithreading](/Glossary/Multithreading.md).

### Actions

Implement a version of the forall loop using an Application Program Interface
(API) that enables multithreading on the CPU. Codee assists the programmer by
providing source code rewriting capabilities using OpenMP compiler directives.

### Relevance

Executing a loop using multithreading on the CPU is one of the ways to speed it
up. Multicore CPUs are widely used in modern computers, but writing
multithreaded code is not straightforward. Essentially, the programmer must
explicitly specify how to execute the loop in vector mode on the hardware, as
well as add the appropriate synchronization to avoid race conditions at runtime.
Typically, minimizing the computational overhead of multithreading is the
biggest challenge to speedup the code.

>**Note**  
>Executing forall loops using multithreading incurs less overhead than in the
>case of scalar reduction loops and sparse reduction loops. The main reason is
>that no synchronization is needed to avoid race conditions and ensure the
>correctness of the code. Note appropriate data scoping of shared and private
>variables is still a must.

### Code example

Have a look at the following code snippet:

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

The code snippet below shows an implementation that uses the OpenMP compiler
directives for multithreading. Note no synchronization is required to avoid race
conditions.

```c
void example(double *D, double *X, double *Y, int n, double a) {
  #pragma omp parallel default(none) shared(D, X, Y, a, n)
  {
    #pragma omp for schedule(auto)
    for (int i = 0; i < n; ++i) {
      D[i] = a * X[i] + Y[i];
    }
  } // end parallel
}
```

### Related resources

* [PWR050 examples at GitHub](/Checks/PWR050)

### References

* [Forall pattern](/Glossary/Patterns-for-performance-optimization/Forall.md)

* [Multithreading](/Glossary/Multithreading.md)
