# PWR043: Consider loop interchange by replacing the scalar reduction

### Issue

Performance of the loop nest can benefit from loop interchange, but scalar
reduction prevents loop interchange.

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
efficient one. Often, loop interchange enables vectorization of the innermost
loop which additionally improves performance.

In order to perform the loop interchange, the loops need to be
[perfectly nested](/Glossary/Perfect-loop-nesting.md), i.e. all the statements
need to be inside the innermost loop. However, due to the initialization of a
reduction variablе, loop interchange is not directly applicable.

### Actions

Replace the scalar reduction by a vector, particularly by the vector that
contains the final result of the original loop. Then, perform loop fission to
isolate the computation of the scalar reduction in a perfect loop nesting and
enable loop interchange. This typically leads to creating two loops: the first
loop initializes the scalar reduction; and the second loop computes the scalar
reduction. All these operations are performed directly on the vector of final
results of the original loop. See below a code example on how it is done.

### Code example

Have a look at the following code:

```c
for (int i = 0; i < n; i++) {
  double s = 0.0;
  for (int j = 0; j < n; j++) {
    s += a[j][i];
  }
  b[i] = s;
}
```

With regards to the innermost loop, the memory access pattern of the matrix `a`
is strided, and this loop can profit from loop interchange, but reduction
variable initialization on line 2 and reduction variable usage on line 6 prevent
it.

To make the loop vectorizable, we remove the temporary scalar value `s` and
replace it with direct writes to `b[i]`:

```c
for (int i = 0; i < n; i++) {
  b[i] = 0.0;
  for (int j = 0; j < n; j++) {
    b[i] += a[j][i];
  }
}
```

After doing this, we can use loop fission to move the statement on line 2 to a
separate loop. The result looks like this:

```c
for (int i = 0; i < n; i++) {
  b[i] = 0.0;
}
for (int i = 0; i < n; i++) {
  for (int j = 0; j < n; j++) {
    b[i] += a[j][i];
  }
}
```

The first loop (line 1) is not performance critical, since it is not nested. On
the other hand, the second loop (line 4) is performance critical, since it
contains the loop nest.

Fortunately, the loop nest is now perfectly nested, so that loop interchange is
applicable. The final result looks like this (note the order of the nested loops
is now `ji` instead of the original order `ij`):

```c
for (int i = 0; i < n; i++) {
  b[i] = 0.0;
}
for (int j = 0; j < n; j++) {
  for (int i = 0; i < n; i++) {
    b[i] += a[j][i];
  }
}
```

### Related resources

* [PWR010: Avoid column-major array access in C/C++](/Checks/PWR010/README.md)

* [PWR039: Consider loop interchange to improve the locality of reference and enable vectorization](/Checks/PWR039/README.md)

* [PWR042: Consider loop interchange by promoting the scalar reduction variable to an array](/Checks/PWR042/README.md)
