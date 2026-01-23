# PWR034: Avoid strided array access to improve performance

### Issue

Strided array access may impact performance.

### Actions

Consider using techniques like loop fusion,
[loop interchange](../../Glossary/Loop-interchange.md),
[loop tiling](../../Glossary/Loop-tiling.md) or changing the data layout to avoid
non-sequential access in hot loops.

### Relevance

Accessing an array using a non-unit stride is less efficient than accessing
consecutive positions because the latter improves the
[locality of reference](../../Glossary/Locality-of-reference.md). Note that C/C++
[column-wise matrix access](../../Glossary/Row-major-and-column-major-order.md)
is an example non-unit stride, where the stride is the column width.

### Code example

#### C

The following code shows a loop with a strided access to array `a` with stride
`2`. Avoiding it would require changing the data layout of the program:

```c
void example(float *a, unsigned size) {
  for (unsigned i = 0; i < size; i += 2) {
    a[i] = 0.0f;
  }
}
```

Another code with strided accesses is show below. In this case, both variables
`a` and `b` have a stride `LEN`:

```c
for (int i = 0; i < LEN; ++i) {
  for (int j = 1; j < LEN; j++) {
    a[j * LEN + i] = a[j * LEN + i - 1] + b[j * LEN + i];
  }
}
```

Note that by using loop interchange, the loop order changes from `ij` to `ji`.
The resulting code shown below has sequential accesses (i.e., stride `1`) for
variables `ij` and `b` in the scope of the innermost loop. As a result, a
simple code change solves the issue in this scenario, without requiring
disruptive changes in data layout:

```c
for (int j = 1; j < LEN; ++j) {
  for (int i = 0; i < LEN; i++) {
    a[j * LEN + i] = a[j * LEN + i - 1] + b[j * LEN + i];
  }
}
```

#### Fortran

The following code shows a loop with a strided access to array `a` with stride
`2`. Avoiding it would require changing the data layout of the program:

```fortran
subroutine example(a)
  real, intent(out) :: a(:)
  integer :: i

  do i = 1, size(a, 1), 2
    a(i) = 0.0
  end do
end subroutine example
```

Another code with strided accesses is show below. In this case, both variables
`a` and `b` have dimensions `(LEN, LEN)`, and thus are implicitly accessed with
a stride of `LEN` elements as Fortran uses column-major order:

```fortran
do i = 1, size(a, 1)
  do j = 2, size(a, 2)
    a(i, j) = a(i, j - 1) + b(i, j)
  end do
end do
```

Note that by using loop interchange, the loop order changes from `ij` to `ji`.
The resulting code shown below has sequential accesses (i.e., stride `1`) in the
scope of the innermost loop. As a result, a simple code change solves the issue
in this scenario, without requiring disruptive changes in data layout:

```fortran
do j = 2, size(a, 2)
  do i = 1, size(a, 1)
    a(i, j) = a(i, j - 1) + b(i, j)
  end do
end do
```

### Related resources

* [PWR034 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR034/)

### References

* [Loop interchange](../../Glossary/Loop-interchange.md)

* [Memory access pattern](../../Glossary/Memory-access-pattern.md) (strided access pattern)
