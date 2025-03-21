# PWR006: Avoid privatization of read-only variables

### Issue

[Read-only variables should be shared](../../Glossary/Variable-scoping-in-the-context-of-OpenMP.md)
instead of private to reduce memory consumption and unnecessary data copying.

### Actions

Set the scope of the read-only variable to shared.

### Relevance

Since a read-only variable is never written to, it can be safely shared without
any risk of race conditions. **Sharing variables is more efficient than
privatizing** them from a memory perspective so it should be favored whenever
possible.

### Code example

#### C

In the following code, arrays `A` and `B` are never written to. However, they
are privatized and thus each thread will hold a copy of each array, effectively
using more memory and taking more time to create private copies:

```c
#define SIZE 5

void example() {
  int A[SIZE] = {1, 2, 3, 4, 5};
  int B[SIZE] = {5, 4, 3, 2, 1};
  int sum[SIZE];

  #pragma omp parallel for shared(sum) firstprivate(A, B)
  for (int i = 0; i < SIZE; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

To save memory, change their scope to shared. This may also prevent memory
issues when using arrays, as codes may easily run out of memory for a high
number of threads:

```c
#define SIZE 5

void example() {
  int A[SIZE] = {1, 2, 3, 4, 5};
  int B[SIZE] = {5, 4, 3, 2, 1};
  int sum[SIZE];

  #pragma omp parallel for shared(sum, A, B)
  for (int i = 0; i < SIZE; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

#### Fortran

In the following code, arrays `A` and `B` are never written to. However, they
are privatized and thus each thread will hold a copy of each array, effectively
using more memory and taking more time to create private copies:

```fortran
subroutine example()
  implicit none
  integer :: i
  integer :: a(5)
  integer :: b(5)
  integer :: sum(5)

  a = [1, 2, 3, 4, 5]
  b = [6, 7, 8, 9, 10]

  !$omp parallel do default(none) firstprivate(a, b) shared(sum) private(i)
  do i = 1, 5
    sum(i) = a(i) + b(i)
  end do
  !$omp end parallel do
end subroutine example
```

To save memory, change their scope to shared. This may also prevent memory
issues when using arrays, as codes may easily run out of memory for a high
number of threads:

```fortran
subroutine example()
  implicit none
  integer :: i
  integer :: a(5)
  integer :: b(5)
  integer :: sum(5)

  a = [1, 2, 3, 4, 5]
  b = [6, 7, 8, 9, 10]

  !$omp parallel do default(none) shared(a, b, sum) private(i)
  do i = 1, 5
    sum(i) = a(i) + b(i)
  end do
  !$omp end parallel do
end subroutine example
```

### Related resources

* [PWR006 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR006/)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked May 2019]

### References

* [Privatization](https://en.wikipedia.org/wiki/Privatization_(computer_programming))

* [Overhead (memory overhead)](https://en.wikipedia.org/wiki/Overhead_(computing))
