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

Consider the example code below to illustrate the presence of non-consecutive
access patterns. The elements of array `a` are accessed in a non-consecutive
manner. In the scope of the outer loop, `for (i)`, all the iterations access
the first row of the array. Thus, the code exhibits repeated accesses to all
the elements of the first row, a total number of times equal to `rows`:

```c
void example(float **a, unsigned rows, unsigned cols) {
  for (unsigned i = 0; i < rows; ++i) {
    for (unsigned j = 0; j < cols; ++j) {
      a[0][j] = 0.0f;
    }
  }
}
```

#### Fortran

Consider the example code below to illustrate the presence of non-consecutive
access patterns. The elements of array `a` are accessed in a non-consecutive
manner. In the scope of the outer loop, `do j`, all the iterations access the
first column of the array. Thus, the code exhibits repeated accesses to all the
elements of the first column, a total number of times equal to `size(a, 2)`:

```f90
subroutine example(a)
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

### Related resources

* [PWR035 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR035/)

### References

* [Memory access pattern](../../Glossary/Memory-access-pattern.md) (non-consecutive array access)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Loop interchange](../../Glossary/Loop-interchange.md)

* [Loop tiling](../../Glossary/Loop-tiling.md)
