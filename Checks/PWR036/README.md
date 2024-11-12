# PWR036: Avoid indirect array access to improve performance

### Issue

[Indirect array access](../../Glossary/Memory-access-pattern.md) may impact
performance.

### Actions

Consider using techniques like loop fusion,
[loop interchange](../../Glossary/Loop-interchange.md),
[loop tiling](../../Glossary/Loop-tiling.md) or changing the data layout to avoid
non-consecutive accesses in hot loops.

### Relevance

Accessing an array indirectly (e.g. through another array containing the
positions to be accessed) is generally less efficient than accessing consecutive
positions because the latter improves
[locality of reference](../../Glossary/Locality-of-reference.md).

### Code example

#### C

Consider the example code below to illustrate the presence of indirect access
patterns. The elements of array `a` are accessed in an indirect manner through
the array `b`. Thus, the code exhibits random accesses that cannot be predicted
before the actual execution of the code:

```c
void example(float *a, unsigned *b, unsigned size) {
  for (unsigned i = 0; i < size; ++i) {
    a[b[i]] = 0.0f;
  }
}
```

Next, consider another example code where memory access patterns can be
optimized to improve locality of reference. The elements of the array `a` are
accessed indirectly through the array `index`. Consequently, the program
accesses random elements of the array `a`, which leads to a low performance due
to a poor usage of the memory subsystem:

```c
for (int i = 0; i < LEN_1D; ++i) {
  for (int j = 1; j < LEN_1D; j++) {
    c[i] += a[index[j]];
  }
}
```

The alternative implementation shown below takes advantage of loop interchange
to improve locality of reference. Now, the loop over `j` becomes the outer
loop, and the loop over `i` becomes the inner loop. As a result, the access to
`a[index[j]]` is repeated across the iterations of the inner loop since the
value of `j` doesn't change, resulting in accesses to a constant memory
location. This leads to a better usage of the memory subsystem, and thus, to a
performance improvement:

```c
for (int j = 1; j < LEN_1D; j++) {
  for (int i = 0; i < LEN_1D; ++i) {
    c[i] += a[index[j]];
  }
}
```

#### Fortran

Consider the example code below to illustrate the presence of indirect access
patterns. The elements of array `a` are accessed in an indirect manner through
the array `b`. Thus, the code exhibits random accesses that cannot be predicted
before the actual execution of the code:

```f90
subroutine example()
  implicit none
  integer, intent(out) :: a
  integer, intent(in) :: b
  integer :: i

  do i = 1, size(a, 1)
    a(b(i)) = 0
  end do
end subroutine example
```

Next, consider another example code where memory access patterns can be
optimized to improve locality of reference. The elements of the array `a` are
accessed indirectly through the array `index`. Consequently, the program
accesses random elements of the array `a`, which leads to a low performance due
to a poor usage of the memory subsystem:

```f90
do i = 1, size(c, 1)
  do j = 2, size(index, 1)
    c(i) = c(i) + a(index(j))
  end do
end do
```

The alternative implementation shown below takes advantage of loop interchange
to improve locality of reference. Now, the loop over `j` becomes the outer
loop, and the loop over `i` becomes the inner loop. As a result, the access to
`a(index(j))` is repeated across the iterations of the inner loop since the
value of `j` doesn't change, resulting in accesses to a constant memory
location. This leads to a better usage of the memory subsystem, and thus, to a
performance improvement:

```f90
do j = 2, size(index, 1)
  do i = 1, size(c, 1)
    c(i) = c(i) + a(index(j))
  end do
end do
```

### Related resources

* [PWR036 examples](../PWR036/)

### References

* [Memory access pattern](../../Glossary/Memory-access-pattern.md) (indirect array access)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Loop interchange](../../Glossary/Loop-interchange.md)

* [Loop tiling](../../Glossary/Loop-tiling.md)
