# PWR042: Consider loop interchange by promoting the scalar reduction variable to an array

### Issue

Performance of the loop nest can benefit from loop interchange, but scalar
reduction prevents loop interchange.

### Actions

Promote scalar reduction to a vector. Then, perform loop fission to isolate the
computation of the scalar reduction in a perfect loop nesting and enable loop
interchange. This typically leads to creating three loops: the first loop
initializes the scalar reduction; the second loop computes the scalar reduction;
and the third loop computes the final result of the original loop.

### Relevance

Inefficient [memory access pattern](../../Glossary/Memory-access-pattern.md) and low
[locality of reference](../../Glossary/Locality-of-reference.md) are among the main
reasons for low performance on modern computer systems. Matrices are
[stored in a row-major order in C and column-major order in Fortran](../../Glossary/Row-major-and-column-major-order.md).
Iterating over them column-wise (in C) and row-wise (in Fortran) is inefficient,
because it uses the memory subsystem suboptimally.

Nested loops that iterate over matrices inefficiently can be optimized by
applying [loop interchange](../../Glossary/Loop-interchange.md). Using loop
interchange, the inefficient matrix access pattern is replaced with a more
efficient one.

In order to perform the loop interchange, the loops need to be
[perfectly nested](../../Glossary/Perfect-loop-nesting.md), i.e. all the statements
need to be inside the innermost loop. However, due to the initialization of a
reduction variablе, loop interchange is not directly applicable.

>**Note**  
>Often, loop interchange enables vectorization of the innermost loop which
>additionally improves performance.

### Code example

#### C

Take a look at the following code. Since C stores the elements of the matrix
`A` in row-major order, the memory access pattern of the innermost loop,
`for(j)`, is strided:

```c
for (int i = 0; i < n; i++) {
  double s = 0.0;

  for (int j = 0; j < n; j++) {
    s += A[j][i];
  }

  B[i] = 0.1 * s;
}
```

The loops could benefit from loop interchange, but this optimization can't be
applied because the loops are not perfectly nested. Note the presence of the
reduction variable initialization (`s = 0.0`) between the loop headers, in
addition to the accesses to array `B` between the loop ends:

To apply loop interchange, the non-perfectly-nested loops must be turned into
perfectly nested loops. This will be achieved by applying several code changes
related to [scalar to vector
promotion](../../Glossary/Scalar-to-vector-promotion.md) on the variable `s`.

First, let's replace the scalar variable `s` with an array:

```c
for (int i = 0; i < n; i++) {
  s[i] = 0.0;

  for (int j = 0; j < n; j++) {
    s[i] += a[j][i];
  }

  B[i] = 0.1 * s[i];
}
```

Next, we apply loop fission to move the statements between the loop headers and
ends into separate loops. The resulting code looks is as follows:

```c
for (int i = 0; i < n; i++) {
  s[i] = 0.0;
}

for (int i = 0; i < n; i++) {
  for (int j = 0; j < n; j++) {
    s[i] += a[j][i];
  }
}

for (int i = 0; i < n; i++) {
  B[i] = 0.1 * s[i];
}
```

Note how the original loop nest is rewritten as three separate sections. The
first and the third loops are single non-nested loops, so let's focus on the
second loop nest as it will have a higher impact on performance.

Note that this loop nest is perfectly nested, making loop interchange
applicable. This optimization will turn the `ij`  order into `ji`, improving
the locality of reference:

```c
for (int i = 0; i < n; i++) {
  s[i] = 0.0;
}
for (int j = 0; j < n; j++) {
  for (int i = 0; i < n; i++) {
    s[i] += a[j][i];
  }
}
for (int i = 0; i < n; i++) {
  b[i] = 0.1 * s[i];
}
```

#### Fortran

Take a look at the following code. Since Fortran stores the elements of the
matrix `A` in column-major order, the memory access pattern of the innermost
loop, `do(j)`, is strided:

```f90
do i = 1, size(A, 1)
  s = 0.0

  do j = 1, size(A, 2)
    s = s + A(i, j)
  end do

  B(i) = 0.1 * s
end do
```

The loops could benefit from loop interchange, but this optimization can't be
applied because the loops are not perfectly nested. Note the presence of the
reduction variable initialization (`s = 0.0`) between the loop headers, in
addition to the accesses to array `B` between the loop ends:

To apply loop interchange, the non-perfectly-nested loops must be turned into
perfectly nested loops. This will be achieved by applying several code changes
related to [scalar to vector
promotion](../../Glossary/Scalar-to-vector-promotion.md) on the variable `s`.

First, let's replace the scalar variable `s` with an array:

```f90
do i = 1, size(A, 1)
  s(i) = 0.0

  do j = 1, size(A, 2)
    s(i) = s(i) + A(i, j)
  end do

  B(i) = 0.1 * s(i)
end do
```

Next, we apply loop fission to move the statements between the loop headers and
ends into separate loops. The resulting code looks is as follows:

```f90
do i = 1, size(A, 1)
  s(i) = 0.0
end do

do i = 1, size(A, 1)
  do j = 1, size(A, 2)
    s(i) = s(i) + A(i, j)
  end do
end do

do i = 1, size(A, 1)
  B(i) = 0.1 * s(i)
end do
```

Note how the original loop nest is rewritten as three separate sections. The
first and the third loops are single non-nested loops, so let's focus on the
second loop nest as it will have a higher impact on performance.

Note that this loop nest is perfectly nested, making loop interchange
applicable. This optimization will turn the `ij`  order into `ji`, improving
the locality of reference:

```f90
do i = 1, size(A, 1)
  s(i) = 0.0
end do

do j = 1, size(A, 2)
  do i = 1, size(A, 1)
    s(i) = s(i) + A(i, j)
  end do
end do

do i = 1, size(A, 1)
  B(i) = 0.1 * s(i)
end do
```

### Related resources

* [PWR042 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR042/)

### References

* [Loop interchange](../../Glossary/Loop-interchange.md)

* [Scalar to vector promotion](../../Glossary/Scalar-to-vector-promotion.md)

* [Loop fission](../../Glossary/Loop-fission.md)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Row-major and column-major order](../../Glossary/Row-major-and-column-major-order.md)

* [Memory access pattern](../../Glossary/Memory-access-pattern.md)
