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

Inefficient [memory access pattern](/Glossary/Memory-access-pattern.md) and low
[locality of reference](/Glossary/Locality-of-reference.md) are among the main
reasons for low performance on modern computer systems. Matrices are
[stored in a row-major order in C and column-major order in Fortran](https://www.appentra.com/knowledge/glossary-row-major-and-column-major-order/).
Iterating over them column-wise (in C) and row-wise (in Fortran) is inefficient,
because it uses the memory subsystem suboptimally.

Nested loops that iterate over matrices inefficiently can be optimized by
applying [loop interchange](/Glossary/Loop-interchange.md). Using loop
interchange, the inefficient matrix access pattern is replaced with a more
efficient one.

In order to perform the loop interchange, the loops need to be
[perfectly nested](/Glossary/Perfect-loop-nesting.md), i.e. all the statements
need to be inside the innermost loop. However, due to the initialization of a
reduction variablе, loop interchange is not directly applicable.

>**Note**  
>Often, loop interchange enables vectorization of the innermost loop which
>additionally improves performance.

### Code example

Have a look at the following code. With regards to the innermost loop `for_j`,
the memory access pattern of the matrix `A` is strided. This loop can profit
from loop interchange, but this optimization technique cannot be applied because
the loops are not perfectly nested. Note the presence of the reduction variable
initialization between the loop headers as well as the presence of accesses to
array `B` between the loop ends.

```c
for (int i = 0; i < n; i++) {
  double s = 0.0;
  for (int j = 0; j < n; j++) {
    s += A[j][i];
  }
  B[i] = 0.1 * s;
}
```

In order to apply loop interchange, the non-perfectly-nested loops must be
turned into perfectly nested loops, through several code changes related to
[scalar to vector promotion](/Glossary/Scalar-to-vector-promotion.md) on
variable `s`. What this means is that we replace the scalar variable `s` with an
array:

```c
for (int i = 0; i < n; i++) {
  s[i] = 0.0;
  for (int j = 0; j < n; j++) {
    s[i] += a[j][i];
  }
  b[i] = 0.1 * s[i];
}
```

After doing this, we use loop fission to move the statements between the loop
headers into a separate loop as well as the statements between the loop ends to
another separate loop. The result looks like this:

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
  b[i] = 0.1 * s[i];
}
```

Now, the original loop nest is rewritten as three separate loop nests. The first
and the third loops are single non-nested loops, so let's focus on the second
loop nest as it has a higher impact on performance. Note this loop nest is
perfectly nested, so loop interchange is applicable and the `ij`  order can be
turned into the `ji` order to improve locality of reference. The final result
looks like this:

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

### Related resources

* [PWR042 examples at GitHub](/Checks/PWR042)

### References

* [Loop interchange](/Glossary/Loop-interchange.md)

* [Scalar to vector promotion](/Glossary/Scalar-to-vector-promotion.md)

* [Loop fission](/Glossary/Loop-fission.md)

* [Locality of reference](/Glossary/Locality-of-reference.md)

* [Row-major and column-major order](/Glossary/Row-major-and-column-major-order.md)

* [Memory access pattern](/Glossary/Memory-access-pattern.md)
