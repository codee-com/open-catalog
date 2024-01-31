# PWR009: Use OpenMP teams to offload work to GPU

### Issue

OpenMP teams should be used to distribute work
[offloaded to the GPU](../../Glossary/Offloading.md).

### Actions

Use the `target teams distribute parallel for` combined construct to offload
work to the GPU using two levels of parallelism.

### Relevance

GPUs are notably different from CPUs, being composed by a high number of
processing units instead of a low level of cores. Moreover, these processing
units are organized following a hierarchy within the GPU that requires some
specific setup in order to better exploit its capabilities.

The OpenMP `parallel` construct specifies a parallel region of the code that
will be executed by a team of threads. It is normally accompanied by a
worksharing construct so that each thread of the team takes care of part of the
work (e.g the `for` construct assigns a subset of the loop iterations to each
thread). This attains a single level of parallelism since all work is
distributed across a team of threads. This works well for multi-core CPUs but
GPUs are composed of a high number of processing units organized into groups
that can share memory and synchronize. This must be taken into account in order
to get the better performance out of GPUs.

The OpenMP `teams distribute` construct can be used to introduce an additional
level of parallelism by creating multiple teams of threads and distributing loop
iterations across them. Each team forms a contention group, meaning that threads
can only synchronize with other threads in its team. This allows the work to be
distributed better fitting the hierarchical organization of the processing units
of GPUs. Additionally, using teams enhances performance portability, ensuring a
more predictable performance no matter which compiler and hardware combination
is used.

### Code example

The following code offloads a matrix multiplication computation through the
`target` construct and then creates a parallel region and distributes the work
through `for` construct (note that the matrices are statically sized arrays):

```c
#pragma omp target map(to: A[0:m][0:p], B[0:p][0:n], m, n, p) \
        map(tofrom: C[0:m][0:n])
{
  #pragma omp parallel default(none) shared(A, B, C, m, n, p)
  {
    #pragma omp for schedule(auto)
    for (size_t i = 0; i < m; i++) {
      for (size_t j = 0; j < n; j++) {
        for (size_t k = 0; k < p; k++) {
          C[i][j] += A[i][k] * B[k][j];
        }
      }
    }
  } // end parallel
} // end target
```

When offloading to the GPU it is recommended to use an additional level of
parallelism. This can be achieved by using the `teams` and `distribute`
constructs, in this case in combination with `parallel for`:

```c
#pragma omp target teams distribute parallel for \
        map(to: A[0:m][0:p], B[0:p][0:n], m, n, p) shared(A, B, m, n, p) \
        map(tofrom: C[0:m][0:n]) schedule(auto)
for (size_t i = 0; i < m; i++) {
  for (size_t j = 0; j < n; j++) {
    for (size_t k = 0; k < p; k++) {
      C[i][j] += A[i][k] * B[k][j];
    }
  }
}
```

### Related resources

* [PWR009 examples](../PWR009)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked June 2020]

* [Portability of OpenMP Offload Directives - Jeff Larkin, OpenMP Booth Talk SC17](https://www.openmp.org/wp-content/uploads/SC17-OpenMPBooth_jlarkin.pdf),
November 2017 [last checked June 2020]

* [OpenMP and NVIDIA - Jeff Larkin, NVIDIA Developer Technologies](https://www.openmp.org/wp-content/uploads/SC13_OpenMP_and_NVIDIA.pdf)
[last checked June 2020]

### References

* [teams distribute construct - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu73.html#x100-3540002.13.11)

* [Overview - Performance Portability](https://performanceportability.org/perfport/overview/)
