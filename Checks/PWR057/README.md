# PWR057: Consider applying offloading parallelism to sparse reduction loop

### Issue

A loop containing the
[sparse reduction](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md)
pattern can be sped up by
[offloading it to an accelerator](../../Glossary/Offloading.md). Codee can do this
automatically, no source code modification is needed by the developer.

### Actions

Implement a version of the sparse reduction loop using an Application Program
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
synchronization to avoid race conditions at runtime. Typically, minimizing the
computational overhead of offloading is the biggest challenge to speedup the
code using accelerators.

>**Note**  
>Offloading
>[sparse reduction](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md)
>loops incurs an overhead due to the synchronization needed to avoid race
>conditions and ensure the correctness of the code. Note appropriate data
>scoping of shared and private variables is still a must.

### Code example

Have a look at the following code snippet:

```c
void example(double *A, int *nodes, int n) {
  for (int nel = 0; nel < n; ++nel) {
    A[nodes[nel]] += nel * 1;
  }
}
```

The loop body has a `sparse reduction`  pattern, meaning that each iteration of
the loop *reduces* its computational result to a value, but the place where the
value is stored is known at runtime only. Thus, two different iterations can
potentially update the same element of the array `A`, which creates a potential
race condition that must be handled through appropriate synchronization.

The code snippet below shows an implementation that uses the OpenACC compiler
directives to offload the loop to an accelerator. Note the synchronization added
to avoid race conditions and the data transfer clauses that manage the data
movement between the host memory and the accelerator memory.

```c
void example(double *A, int *nodes, int n) {
  #pragma acc data copyin(n, nodes[0:n]) copy(A[:])
  #pragma acc parallel
  #pragma acc loop
  for (int nel = 0; nel < n; ++nel) {
    #pragma acc atomic update
    A[nodes[nel]] += nel * 1;
  }
}
```

### Related resources

* [PWR057 examples](../PWR057)

### References

* [Sparse reduction pattern](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md)

* [Offloading](../../Glossary/Offloading.md)
