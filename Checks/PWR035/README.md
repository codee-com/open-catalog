# PWR035: avoid non-consecutive array access to improve performance

### Issue

[Non-consecutive array access](../../Glossary/Memory-access-pattern.md) may impact
performance.

### Actions

Consider using techniques like loop fusion,
[loop interchange](../../Glossary/Loop-interchange.md),
[loop tiling](../../Glossary/Loop-tiling.md)
or changing the data layout to avoid non-consecutive access in hot loops.

### Relevance

Accessing an array in a non-consecutive order is less efficient than accessing
consecutive positions because the latter maximises
[locality of reference](../../Glossary/Locality-of-reference.md).

### Code example

#### C

The example code below illustrates a non-consecutive array access pattern in a
two-dimensional array:

```c
void example(float **a, unsigned rows, unsigned cols) {
  for (unsigned i = 0; i < rows; ++i) {
    for (unsigned j = 0; j < cols; ++j) {
      a[0][j] = 0.0f;
    }
  }
}
```

At first glance, the inner loop `for (j)` appears to access the elements of the
array sequentially. However, the outer loop `for (i)` is repeatedly accessing
the first row of the array. This results in a non-consecutive memory access
pattern when considering the computation as a whole.

CPUs rely on caches to reduce the cost of memory accesses. Instead of loading
data from main memory element by element, CPUs read fixed-size blocks of data
called cache lines (e.g., 64 bytes on `x86`). When a code accesses memory, the
CPU fetches the entire cache line containing the requested data, making
sequential memory accesses highly efficient due to spatial locality.

In C, arrays are stored in row-major order, meaning that elements of a row are
laid out sequentially in memory, followed by subsequent rows. This organization
makes row-by-row processing naturally cache-friendly. However, the example code
disrupts the spatial locality by repeatedly returning to the start of the first
row.

When working with large matrices that exceed the cache's capacity, this pattern
can become particularly inefficient. Cache lines holding the start of the first
row may be evicted to make room for subsequent data. As a result, the code may
repeatedly reload already-seen lines from main memory, incurring a significant
overhead.

> [!TIP]
> While modern CPUs implement prefetching mechanisms to understand and
> anticipate the memory access patterns of a code, they are not foolproof.

#### Fortran

The example code below illustrates a non-consecutive array access pattern in a
two-dimensional array:

```fortran
pure subroutine example(a)
  implicit none
  integer, intent(out) :: a(:, :)
  integer :: i, j

  do j = 1, size(a, 2)
    do i = 1, size(a, 1)
      a(i, 1) = 0
    end do
  end do
end subroutine example
```

At first glance, the inner loop `do i` appears to access the elements of the
array sequentially. However, the outer loop `do j` is repeatedly accessing the
first column of the array. This results in a non-consecutive memory access
pattern when considering the computation as a whole.

CPUs rely on caches to reduce the cost of memory accesses. Instead of loading
data from main memory element by element, CPUs read fixed-size blocks of data
called cache lines (e.g., 64 bytes on `x86`). When a code accesses memory, the
CPU fetches the entire cache line containing the requested data, making
sequential memory accesses highly efficient due to spatial locality.

In Fortran, arrays are stored in column-major order, meaning that elements of a
column are laid out sequentially in memory, followed by subsequent columns.
This organization makes column-by-column processing naturally cache-friendly.
However, the example code disrupts the spatial locality by repeatedly returning
to the start of the first column.

When working with large matrices that exceed the cache's capacity, this pattern
can become particularly inefficient. Cache lines holding the start of the first
column may be evicted to make room for subsequent data. As a result, the code
may repeatedly reload already-seen lines from main memory, incurring a
significant overhead.

> [!TIP]
> While modern CPUs implement prefetching mechanisms to understand and
> anticipate the memory access patterns of a code, they are not foolproof.

### Related resources

* [PWR035 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR035/)

### References

* [Memory access pattern](../../Glossary/Memory-access-pattern.md) (non-consecutive array access)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Loop interchange](../../Glossary/Loop-interchange.md)

* [Loop tiling](../../Glossary/Loop-tiling.md)
