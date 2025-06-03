# Scalar reduction pattern

A scalar reduction combines multiple values into one single element (the scalar
reduction variable) by applying an associative, commutative operator. The
pattern is often represented by a loop: 

```c
for (int i = 0; i < N; ++i) {
  v = v ⨁ ...
}
```

`v` is the scalar reduction variable and `⨁` is the associative and commutative
reduction operator that guarantees that the order in which the computations are
performed will not alter the final result of the reduction operation. 

### Code examples

#### C

```c
for (int i = 0; i < N; ++i) {
  sum += A[i];
}
```

#### Fortran

```fortran
do i = 1, n
  sum = sum + A(i)
end do
```

### Parallelizing scalar reductions with OpenMP and OpenACC

The computation of the scalar reduction has concurrent read-write accesses to
the scalar reduction variable. Therefore a scalar reduction can be computed in
parallel safely only if additional synchronization is inserted in order to avoid
race conditions associated to the reduction variable. 

Scalar reductions can be parallelized in multiples ways, including: 

1. Use a built-in OpenMP/OpenACC reduction.

2. Parallelize across loop iterations, but calculate the reduction within an
atomic or critical region.

3. Parallelize the loop by creating a private copy of the reduction variable for
each thread. The loop calculation is then followed by a separate reduction using
an atomic operation. 
