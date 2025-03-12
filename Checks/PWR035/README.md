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

The example code below illustrates a non-consecutive access pattern on a
one-dimensional array. Each iteration of the loop accesses two elements located
far apart in memory; one from the beginning and one from the end of the array:

```c
void reverseArray(float *array, unsigned size) {
  for (unsigned i = 0; i < size / 2; ++i) {
    float temp = array[i];
    array[i] = array[size - i - 1];
    array[size - i - 1] = temp;
  }
}
```

Modern CPUs use prefetching mechanisms to predict and load upcoming memory
accesses into the cache. However, these mechanisms are not foolproof,
particularly when the access pattern is non-consecutive, such as the
alternating access shown above. In such cases, the predictions might be
incorrect, leading to inefficient use of the memory subsystem and degraded
performance.

#### Fortran

The example code below illustrates a non-consecutive access pattern on a
one-dimensional array. Each iteration of the loop accesses two elements located
far apart in memory; one from the beginning and one from the end of the array:

```fortran
pure subroutine reverseArray(array)
  implicit none
  integer, intent(inout) :: array(:)
  integer :: i, length, temp

  length = size(array)

  do i = 1, length / 2
    temp = array(i)
    array(i) = array(length - i + 1)
    array(length - i + 1) = temp
  end do
end subroutine reverseArray
```

Modern CPUs use prefetching mechanisms to predict and load upcoming memory
accesses into the cache. However, these mechanisms are not foolproof,
particularly when the access pattern is non-consecutive, such as the
alternating access shown above. In such cases, the predictions might be
incorrect, leading to inefficient use of the memory subsystem and degraded
performance.

### Related resources

* [PWR035 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR035/)

### References

* [Memory access pattern](../../Glossary/Memory-access-pattern.md) (non-consecutive array access)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Loop interchange](../../Glossary/Loop-interchange.md)

* [Loop tiling](../../Glossary/Loop-tiling.md)
