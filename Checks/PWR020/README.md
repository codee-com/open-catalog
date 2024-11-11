# PWR020: Consider loop fission to enable vectorization

### Issue

The loop can be partially vectorized straightforwardly by moving the
non-vectorizable statements of the loop body to a separate loop.

### Actions

Rewrite the loop to enable partial vectorization by separating the vectorizable
statements in a first loop and the non-vectorizable statements in a second loop.

### Relevance

[vectorization](../../Glossary/Vectorization.md) is one of the most important ways to
speed up the computation of a loop. In practice, loops may contain a mix of
computations where only a part of the loop body introduces loop-carrie
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
and recurrence compute patterns to a separate loop. Frequently, loop fission can
be implemented in a straightforward manner because there are not any temporary
scalar variables in the loop body.

### Code examples

#### C

The second loop in the following example exhibits a forall and a sparse
reduction compute pattern for arrays `A` and `B`, respectively. The sparse
reduction introduces loop-carried dependencies that inhibit loop vectorization.

```c
void example() {
  int A[1000], B[1000], C[1000];

  for (int i = 0; i < 1000; i++) {
    A[i] = B[i] = C[i] = i;
  }

  for (int i = 0; i < 1000; i++) {
    A[i] += i;
    B[C[i]] += i;
  }
}
```

After applying loop fission, the original loop is split into two loops: first
loop computes the vectorizable part of the loop body (the forall pattern), and
second loop computes the non-vectorizable part in scalar mode (the sparse
reduction).

```c
void example() {
  int A[1000], B[1000], C[1000];

  for (int i = 0; i < 1000; i++) {
    A[i] = B[i] = C[i] = i;
  }

  for (int i = 0; i < 1000; i++) {
    A[i] += i;
  }

  for (int i = 0; i < 1000; i++) {
    B[C[i]] += i;
  }
}
```

#### Fortran

The second loop in the following example exhibits a forall and a sparse
reduction compute pattern for arrays `a` and `b`, respectively. The sparse
reduction introduces loop-carried dependencies that inhibit loop vectorization.

```f90
subroutine example()
  implicit none
  integer :: a(100), b(100), c(100), i

  do i = 1, 100
    a(i) = i
    b(i) = i
    c(i) = i
  end do

  do i = 1, 100
    a(i) = a(i) + i
    b(c(i)) = b(c(i)) + i
  end do
end subroutine example
```

After applying loop fission, the original loop is split into two loops: first
loop computes the vectorizable part of the loop body (the forall pattern), and
second loop computes the non-vectorizable part in scalar mode (the sparse
reduction).

```f90
subroutine example()
  implicit none
  integer :: a(100), b(100), c(100), i

  do i = 1, 100
    a(i) = i
    b(i) = i
    c(i) = i
  end do

  do i = 1, 100
    a(i) = a(i) + i
  end do

  do i = 1, 100
    b(c(i)) = b(c(i)) + i
  end do
end subroutine example
```

### Related resources

* [PWR020 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR020/)

### References

* [Loop fission](../../Glossary/Loop-fission.md)

* [Vectorization](../../Glossary/Vectorization.md)

* [Sparse reduction compute pattern](../../Glossary/Patterns-for-performance-optimization/Sparse-reduction.md)

* [Recurrence compute pattern](../../Glossary/Patterns-for-performance-optimization/Recurrence.md)

* [Scalar reduction compute pattern](../../Glossary/Patterns-for-performance-optimization/Scalar-reduction.md)

* [Forall compute pattern](../../Glossary/Patterns-for-performance-optimization/Forall.md)
