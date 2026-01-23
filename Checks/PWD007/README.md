# PWD007: Unprotected multithreading recurrence

### Issue

An unprotected multithreading recurrence in parallel code is causing a data
race.

### Actions

Protect the recurrence or execute the code sequentially if that is not possible.

### Relevance

The recurrence computation pattern occurs when the same memory position is read
and written to, at least once, in different iterations of a loop. It englobes
both true dependencies (read-after-write) and anti-dependencies (write-after-
read) across loop iterations. Sometimes the term "loop-carried dependencies" is
also used. If a loop with a recurrence computation pattern is parallelized
without protecting the concurrent memory access, a data race condition is
introduced. In some cases, the concurrent memory access can not be protected and
thus the loop can not be parallelized.

### Code example

#### C

The following code performs an exclusive scan, naively parallelized using
multithreading:

```c
void foo(int *x, int *y, int size) {
  y[0] = 0;

  #pragma omp parallel for shared(x, y, size)
  for (int i = 1; i < size; i++) {
    y[i] = y[i - 1] + x[i - 1];
  }
}
```

This approach is incorrect due to the dependence between two consecutive
iterations of the loop. Note how the value `y(i)` is calculated based on the
previous `y(i - 1)`.

Since OpenMP 5.0, this type of computation can be conveniently parallelized
using the `scan` directive:

```c
void foo(int *x, int *y, int size) {
  int scan_x = 0;

  #pragma omp parallel for reduction(inscan, +:scan_x) shared(x, y, size)
  for (int i = 0; i < size; i++) {
    y[i] = scan_x;
    #pragma omp scan exclusive(scan_x)
    scan_x += x[i];
  }
}
```

#### Fortran

The following code performs an exclusive scan, naively parallelized using
multithreading:

```fortran
subroutine foo(x, y)
  implicit none
  integer, intent(in) :: x(:)
  integer, intent(inout) :: y(:)
  integer :: i

  y(1) = 0

  !$omp parallel do private(i) shared(x, y)
  do i = 2, size(y, 1)
    y(i) = y(i - 1) + x(i - 1)
  end do
  !$omp end parallel do
end subroutine foo
```

This approach is incorrect due to the dependence between two consecutive
iterations of the loop. Note how the value `y(i)` is calculated based on the
previous `y(i - 1)`.

Since OpenMP 5.0, this type of computation can be conveniently parallelized
using the `scan` directive:

```fortran
subroutine foo(x, y)
  implicit none
  integer, intent(in) :: x(:)
  integer, intent(out) :: y(:)
  integer :: scan_x, i

  scan_x = 0

  !$omp parallel do private(i) reduction(inscan, +:scan_x) shared(x, y)
  do i = 1, size(y, 1)
    y(i) = scan_x
    !$omp scan exclusive(scan_x)
    scan_x = scan_x + x(i)
  end do
  !$omp end parallel do
end subroutine foo
```

### Related resources

* [PWD007 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWD007/)

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
