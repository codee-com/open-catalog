# Sparse reduction pattern

A sparse or irregular reduction combines a set of values from a subset of the
elements of a vector or array into a set of elements by applying an associative,
commutative operator. The result of this reduction process is stored in an array
reduction variable. The set of array elements used cannot be determined until
runtime due to the use of subscript array to provide these values. The pattern
is often found in a loop: 

```c
for (int i = 0; i < N; ++i) {
  A[B[i]] = A[B[i]] ⨁ ...
} 
```

`A` is the array reduction variable and `⨁` is the associative and commutative
reduction operator that guarantees that the order in which the computations are
performed will not alter the final result of the reduction operation. A key
feature of this pattern is the usage of elements of the array `B`, where `i`
cannot be determined until runtime. 

### Code examples

The code sums contributions of a triangular finite element mesh to compute the
numerical solution of all the nodes of the mesh. The subscript arrays `nodes1`,
`nodes2` and `nodes3` represent the nodes of the mesh that define the geometry
of the triangular finite elements. 

#### C

```c
for (int nel = 0; nel < nelements; ++nelements) {
  A[nodes1[nel]] += elemental_contribution(nel);
  A[nodes2[nel]] += elemental_contribution(nel);
  A[nodes3[nel]] += elemental_contribution(nel);
}
```

#### Fortran

```f90
do nel = 1, nelements
  A(nodes1(nel)) = A(nodes1(nel)) + elemental_contribution(nel)
  A(nodes2(nel)) = A(nodes2(nel)) + elemental_contribution(nel)
  A(nodes3(nel)) = A(nodes3(nel)) + elemental_contribution(nel)
end do
```

### Parallelizing sparse reductions with OpenMP or OpenACC

There is no built-in support for sparse reductions in OpenMP/OpenACC, and thus
parallelization must be performed by explicit control of the variables.

Sparse reductions can be parallelized in multiples ways, including: 

1. Parallelize across loop iterations, but calculate the reduction within an
atomic or critical region.

2. Parallelize the loop by creating a private copy of the reduction variable for
each thread. The loop calculation is then followed by a separate reduction using
an atomic operation.
