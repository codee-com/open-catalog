# PWD007: Unprotected multithreading recurrence

### Issue

An unprotected multithreading recurrence in parallel code is causing a data
race.

### Actions

Protect the recurrence or execute the code sequentially if that is not possible.

### Relevance

The recurrence computation pattern occurs when the same memory position is read
and written to, at least once, in different  iterations of a loop. It englobes
both true dependencies (read-after-write) and anti-dependencies (write-after-
read) across loop iterations. Sometimes the term "loop-carried dependencies" is
also used. If a loop with a recurrence computation pattern is parallelized
without protecting the concurrent memory access, a data race condition is
introduced. In some cases, the concurrent memory access can not be protected and
thus the loop can not be parallelized.

### Code example

The following code performs an exclusive scan, naively parallelized using
multithreading:

```c
void foo() {
  int x[10], y[10];

  y[0] = 0;
  #pragma omp parallel for
  for (int i = 1; i < 10; i++) {
    y[i] = y[i - 1] + x[i - 1];
  }
}
```

This is incorrect due to the recurrence pattern present in the code, in the form
dependencies between two consecutive loop iterations. For instance, it can be
safely implemented in OpenMP 5.0 using the `scan` directive:

```c
void foo() {
  int x[10], y[10];

  int scan_x = 0;
  #pragma omp parallel for reduction(inscan, +:scan_x)
  for (int i = 0; i < 10; i++) {
    y[i] = scan_x;
    #pragma omp scan exclusive(scan_x)
    scan_x += x[i];
  }
}
```

### Related resources

* [PWD007 examples](../PWD007)

* [scan Directive - OPENMP API Specification: Version 5.0](https://www.openmp.org/spec-html/5.0/openmpsu45.html)
[last checked October 2020]

* [OpenMP* SIMD for Inclusive/Exclusive Scans](https://software.intel.com/content/www/us/en/develop/articles/openmp-simd-for-inclusiveexclusive-scans.html)
[last checked October 2020]

### References

* [Recurrence pattern](../../Glossary/Patterns-for-performance-optimization/Recurrence.md)

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
[last checked October 2020]

* [Inclusive and exclusive scans - Prefix sum](https://en.wikipedia.org/wiki/Prefix_sum#Inclusive_and_exclusive_scans)
[last checked October 2020]
