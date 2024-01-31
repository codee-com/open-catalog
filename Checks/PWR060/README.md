# PWR060: Consider loop fission to separate gather memory access pattern

### Issue

The loop can be partially vectorized by moving a *gather* memory access pattern
to a separate loop that stores the values in consecutive memory locations.

### Actions

Rewrite the loop to enable partial [vectorization](../../Glossary/Vectorization.md)
by moving the gather [memory access pattern](../../Glossary/Memory-access-pattern.md)
to a first loop and the rest of the loop body in a second loop. Next, store the
gathered data in consecutive memory locations using a temporary vector that is
written in the first loop and read in the second loop.

### Relevance

Vectorization is one of the most important ways to speed up computation in the
loop. In practice, loops may contain vectorizable statements, but vectorization
may be either inhibited or inefficient due to the usage of data stored in non-
consecutive memory locations. Programs exhibit different types of
[memory access patterns](../../Glossary/Memory-access-pattern.md) that lead to
non-consecutive memory access, e.g. strided, indirect, random accesses.

Thus, [loop fission](../../Glossary/Loop-fission.md) enables the partial
vectorization by moving the gather memory access pattern to a separate loop. It
computes a temporary array that stores the used data in consecutive memory
locations. After loop fission, we end up with two loops. The first loop computes
the gathers and will not be vectorized. The second loop computes the remainder
of the original loop and will be vectorized.

>**Note**  
>Loop fission introduces additional memory overheads, which is needed to
>allocate, read/write and deallocate the memory of the temporary vector that
>stores the used data in consecutive memory locations. The implementation of
>loop fission must take this into consideration to produce performant code.

### Code examples

#### C

Have a look at the following loop. This computationally-intensive loop might
benefit from vectorization, but the loop will not be vectorized because the
gather memory access pattern to array X is reading non-consecutive memory
locations.

```c
for (int i = 0; i < n; ++i) {
  D[i] = a * X[index[i]] + Y[i]
}
```

After applying loop fission, the original loop is split into two loops. The
first loop gathers the non-consecutive data and stores the values in consecutive
storage using the array `X_index_i`. The second loop uses the data from that
array, and the compiler will be able to vectorize the second loop. Overall, the
original loop is partially vectorized through loop fission.

```c
for (int i = 0; i < n; ++i) {
  X_index_i[i] = X[index[i]];
}
for (int i = 0; i < n; ++i) {
  D[i] = a * X_index_i[i] + Y[i]
}
```

#### Fortran

Have a look at the following loop. This computationally-intensive loop might
benefit from vectorization, but the loop will not be vectorized because the
gather memory access pattern to array `X` is reading non-consecutive memory
locations.

```f90
do i = 1, n
  D(i) = a * X(index(i)) + Y(i)
end do
```

After applying loop fission, the original loop is split into two loops. The
first loop gathers the non-consecutive data and stores the values in consecutive
storage using the array `X_index_i`. The second loop uses the data from that
array, and the compiler will be able to vectorize the second loop. Overall, the
original loop is partially vectorized through loop fission.

```f90
real(kind=8), dimension(n) :: X_index_i

do i = 1, n
  X_index_i(i) = X(index(i))
end do
do i = 1, n
  D(i) = a * X_index_i(i) + Y(i)
end do
```

### Related resources

* [PWR060 examples](../PWR060)

### References

* [Loop fission](../../Glossary/Loop-fission.md)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Memory access pattern](../../Glossary/Memory-access-pattern.md)

* [Vectorization](../../Glossary/Vectorization.md)
