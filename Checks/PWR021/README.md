# PWR021: Consider loop fission with scalar to vector promotion to enable vectorization

### Issue

The loop can be partially vectorized by promoting temporary
[scalar variables to vectors](../../Glossary/Scalar-to-vector-promotion.md), moving
the non-vectorizable statements of the loop body to a separate loop.

### Actions

Rewrite the loop to enable partial vectorization by separating the vectorizable
statements in a first loop and the non-vectorizable statements in a second loop,
and by promoting a temporary scalar variable to a vector that is written in the
first loop and read in the second loop.

### Relevance

[Vectorization](../../Glossary/Vectorization.md) is one of the most important ways to
speed up the computation of a loop. I practice, loops may contain a mix of
computations where only a part of the loop body introduces loop-carried
dependencies that prevent vectorization. Different types of compute patterns
make explicit the loop-carried dependencies present in the loop. On the one
hand, the
[forall compute pattern](../../Glossary/Patterns-for-performance-optimization/Forall.md)
is free of loop-carried dependencies and can be vectorized. On the other hand,
the following compute patterns have loop-carried-dependencies and cannot be
vectorized:

* The
[sparse reduction compute pattern](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md) - e.g.
the reduction variable has an read-write indirect memory access pattern which
does not allow to determine the dependencies between the loop iterations at
compile-time.

* The
[recurrence compute pattern](../../Glossary/Patterns-for-performance-optimization/Recurrence.md) - e.g.
typically computes the value of an array element using the value of another
array element calculated in a previous loop iteration.

> [!NOTE]
> The
> [scalar reduction compute pattern](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)
> is kind of an edge case, it introduces loop-carried dependencies, but
> processors often support vector instructions for reductions by hardware.
> However, there are situations where it may be preferable to execute reduction
> operations with scalar instructions. An example is a hardware processor that
> lacks vector instructions for specific reduction operations. Another example is
> a code where it is key to find a trade-off between performance and precision
> control, avoiding round-off errors in floating point operations. Note loop
> fission is a solution that still enables the partial vectorization of part of
> the loop.

Thus, [loop fission](../../Glossary/Loop-fission.md) enables the partial
vectorization by moving the non-vectorizable statements of the sparse reduction
and recurrence compute patterns to a separate loop. Frequently, loop fission
cannot be implemented in a straightforward manner because the loop contains
temporary scalar variables that are used in the non-vectorizable statements.

In the situation described above, in order to perform the loop fission,
temporary scalar values in the loop need to be stored in a vector. After loop
fission, we end up with two loops, and the second loop is depending on the data
generated in the first loop. By promoting temporary scalar values to a vector,
we preserve the values calculated by the first loop to be used in a second loop.

> [!NOTE]
> Loop fission introduces additional memory overheads, which is needed to
> allocate, read/write and deallocate the memory of the vector resulting from the
> promotion of the temporary scalar variable. The implementation of loop fission
> must take this into consideration to produce performant code.

### Code examples

#### C

Have a look at the loop shown below. This computationally-intensive loop may
benefit from vectorization, but it is prevented by the indirect memory accesses
`A[C[i]]` of the sparse reduction compute pattern.

```c
int expensive_computation(int *C, int i) {
    return C[i] * 2;
}

void example(int *A, int *C) {
  for (int i = 0; i < 1000; i++) {
    int t = expensive_computation(C, i);
    A[C[i]] += t;
  }
}
```

After applying loop fission, the original loop is split into two loops: first
loop computes the vectorizable part of the loop body in vector mode, and second
loop computes the sparse reduction in scalar mode. Note that a new auxiliary
array `t` is created to store all the results of the first loop, so that those
values can be used as inputs in the second loop.

```c
int expensive_computation(int *C, int i) {
  return C[i] * 2;
}

void example(int *A, int *C) {
  int t[1000];
  for (int i = 0; i < 1000; i++) {
    t[i] = expensive_computation(C, i);
  }

  for (int i = 0; i < 1000; i++) {
    A[C[i]] += t[i];
  }
}
```

#### Fortran

Have a look at the loop shown below. This computationally-intensive loop may
benefit from vectorization, but it is prevented by the indirect memory accesses
`a(c(i))` of the sparse reduction pattern.

```fortran
integer function expensive_computation(c, i)
  implicit none
  integer, intent(in) :: i, c(1000)

  expensive_computation = c(i) * 2
end function expensive_computation

subroutine example()
  implicit none
  integer :: a(1000), c(1000), i, t, expensive_computation

  do i = 1, 1000
    t = expensive_computation(c, i)
    a(c(i)) = a(c(i)) + t
  end do
end subroutine example
```

After applying loop fission, the original loop is split into two loops: first
loop computes the vectorizable part of the loop body in vector mode, and second
loop computes the sparse reduction in scalar mode. Note that a new auxiliary
array `t` is created to store all the results of the first loop, so that those
values can be used as inputs in the second loop.

```fortran
integer function expensive_computation(c, i)
  implicit none
  integer, intent(in) :: i, c(1000)

  expensive_computation = c(i) * 2
end function expensive_computation

subroutine example()
  implicit none
  integer :: a(1000), c(1000), t(1000), i, b, expensive_computation

  do i = 1, 1000
    t(i) = expensive_computation(c, i)
  end do
  do i = 1, 1000
    a(c(i)) = a(c(i)) + t(i)
  end do
end subroutine example
```

### Related resources

* [PWR021 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR021/)

### References

* [Loop fission](../../Glossary/Loop-fission.md)

* [Vectorization](../../Glossary/Vectorization.md)

* [Sparse reduction compute pattern](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md)
