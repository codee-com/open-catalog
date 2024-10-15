# PWR040: Consider loop tiling to improve the locality of reference

### Issue

Inefficient
[matrix access pattern](../../Glossary/Row-major-and-column-major-order.md) detected
that can be fixed through loop tiling.

### Actions

Apply [loop tiling](../../Glossary/Loop-tiling.md) to the loop nest.

### Relevance

Inefficient [memory access patterns](../../Glossary/Memory-access-pattern.md) and low
[locality of reference](../../Glossary/Locality-of-reference.md) are the main reasons
for low performance on modern computer systems. Matrices are
[stored in a row-major order in C and column-major order in Fortran](../../Glossary/Row-major-and-column-major-order.md).
Iterating over them column-wise (in C) and row-wise (in Fortran) is inefficient,
because it uses the memory subsystem suboptimally.

Nested loops that iterate over matrices in an inefficient manner can be
optimized by applying [loop tiling](../../Glossary/Loop-tiling.md). In contrast to
[loop interchange](../../Glossary/Loop-interchange.md), loop tiling doesn't remove
the inefficient memory access, but instead breaks the problem into smaller
subproblems. Smaller subproblems have a much better
[locality of reference](../../Glossary/Locality-of-reference.md) and are faster to
solve. Using loop tiling, the pressure on the memory subsystem due to
inefficient matrix access is decreased which leads to improvement in program
speed.

>**Note**  
>The benefit of loop tiling directly depends on the size of the dataset. Large
>datasets profit from loop tiling a lot, in contrast to small datasets that
>don't profit that much.

### Code example

#### C

The following code shows two nested loops. The matrix `B` is accessed
[column-wise](../../Glossary/Row-major-and-column-major-order.md), which is
inefficient. [Loop interchange](../../Glossary/Loop-interchange.md) doesn't help
either, because fixing the inefficient memory access pattern for `B` would
introduce an inefficient memory access pattern for `A`:

```c
void example(double **A, double **B, int n) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      A[i][j] = B[j][i];
    }
  }
}
```

After applying loop tiling, the locality of reference is improved and the
performance is better. The tiled version of this loop nest is as follows:

```c
for (int ii = 0; ii < n; ii += TILE_SIZE) {
  for (int jj = 0; jj < n; jj += TILE_SIZE) {
    for (int i = ii; i < MIN(ii + TILE_SIZE, n); i++) {
      for (int j = jj; j < MIN(jj + TILE_SIZE, n); j++) {
        A[i][j] = B[j][i];
      }
    }
  }
}
```

#### Fortran

The following code shows two nested loops. The matrix `B` is accessed
[row-wise](../../Glossary/Row-major-and-column-major-order.md), which is
inefficient. [Loop interchange](../../Glossary/Loop-interchange.md) doesn't
help either, because fixing the inefficient memory access pattern for `B` would
introduce an inefficient memory access pattern for `A`:

```f90
subroutine example(a, b)
  implicit none
  real, dimension(:, :), intent(out) :: a
  real, dimension(:, :), intent(in) :: b
  integer :: i, j

  do j = 1, size(a, 2)
    do i = 1, size(a, 1)
      a(i, j) = b(j, i)
    end do
  end do
end subroutine example
```

After applying loop tiling, the locality of reference is improved and the
performance is better. The tiled version of this loop nest is as follows:

```f90
do jj = 1, size(a, 2), TILE_SIZE
  do ii = 1, size(a, 1), TILE_SIZE
    do j = jj, MIN(jj + TILE_SIZE, size(a, 2))
      do i = ii, MIN(ii + TILE_SIZE, size(a, 1))
        a(i, j) = b(j, i)
      end do
    end do
  end do
end do
```

### Related resources

* [PWR040 examples](../PWR040)

### References

* [Loop tiling](../../Glossary/Loop-tiling.md)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Row-major and column-major order](../../Glossary/Row-major-and-column-major-order.md)

* [Memory access pattern](../../Glossary/Memory-access-pattern.md)
