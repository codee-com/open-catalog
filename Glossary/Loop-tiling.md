# Loop tiling

**Loop tiling** (also known as **loop blocking**) is a program optimization
technique whose aim is to improve
[locality of reference](/Glossary/Locality-of-reference.md). It modifies the
memory access pattern of the loop in order to reuse data already present in the
data cache before that data gets evicted and needs to be reloaded from the
memory.

The basic idea behind loop tiling is to split the processing of data into
smaller segments called tiles or blocks. Because blocks are smaller than the
original workset, they fit into data caches more readily.

Let's consider the following example:

```c
for (int i = 0; i < n; i++) {
  for (int j = 0; j < m; j++) {
    c[i][j] = a[i] * b[j];
  }
}
```

If you look more closely, the whole array `b` needs to be fetched from the
memory `n` times. This is not a problem if the size of the array `b` is small,
since then the values for `b` would be brought up from the main memory to the
data cache once and reused every time after that.

The problem happens when the array `b` is large. In that case, the CPU would
need to bring the values of the array `b` `n` times from the main memory,
since `b` doesn't fit the data cache.

The solution to this problem is loop tiling: instead of running the inner loop
over `j` from 0 to `m` and then increasing the variable `i`, we pick a constant
called `TILE_SIZE` and run the loop according to this pattern:

| `i`         | `j`                                                    |
|-------------|--------------------------------------------------------|
| `i = 0`     | `j = 0, 1, ..., TILE_SIZE - 1`                         |
| `i = 1`     | `j = 0, 1, ..., TILE_SIZE - 1`                         |
| `...`       | `...`                                                  |
| `i = n - 1` | `j = 0, 1, ..., TILE_SIZE - 1`                         |
| `i = 0`     | `j = TILE_SIZE, TILE_SIZE + 1, ..., 2 * TILE_SIZE - 1` |
| `i = 1`     | `j = TILE_SIZE, TILE_SIZE + 1, ..., 2 * TILE_SIZE - 1` |
| `...`       | `...`                                                  |

By running the loop like this, we are reusing the part of the array `b` which is
already in the data cache.

We can rewrite the code to use loop tiling:

```c
for (int jj = 0; jj < m; jj += TILE_SIZE) {
  for (int i = 0; i < n; i++) {
    for (int j = jj; j < MIN(jj + TILE_SIZE, m); j++) {
      c[i][j] = a[i] * b[j];
    }
  }
}
```

The careful reader might notice that after this intervention, the values for the
array `a` will be read `m / TILE_SIZE` times from the memory. If the size of
array `a` is large, then it can be useful to perform loop tiling on the loop
over `i` a as well, like this:

```c
for (int ii = 0; ii < n; ii += TILE_SIZE_I) {
  for (int jj = 0; jj < m; jj += TILE_SIZE_J) {
    for (int i = ii; i < MIN(n, ii + TILE_SIZE_I); i++) {
      for (int j = jj; j < MIN(m, jj + TILE_SIZE_J); j++) {
        c[i][j] = a[i] * b[j];
      }
    }
  }
}
```

Originally, data was processed inside a one large tile with dimensions `n * m`.
After loop tiling, data is processed inside many smaller tiles with dimensions
`TILE_SIZE_I * TILE_SIZE_J`. This kind of processing results in a better usage
of the memory subsystem.

Picking the values for tile size is typically done experimentally. Start off
with some value, e.g. 16, and then change the value until the best performance
is achieved. 

### When should loop tiling be applied?

There are two distinct scenarios where loop tiling is applied:

1. Iterating over the same dataset several times: this happens in the example
above and we showed how loop tiling can help make the code run faster by
shrinking the size of the workload in order to use the data while it is still in
the data cache.

2. Cases with strided and random memory access patterns that cannot be improved
with loop interchange because, with loop interchange, we fix one bad memory
access pattern but introduce a new one: if the algorithm requires a strided
memory access pattern for example, it is possible to optimize the loop with such
a memory access pattern using loop tiling, even if there is no data reloading.

We already covered an example for (1) earlier, and loop tiling for (2) requires
additional explanation.

Let's take as an example matrix transposition:

```c
for (int i = 0; i < n; i++) {
  for (int j = 0; j < n; j++) {
    b[i][j] = a[j][i];
  }
}
```

In this example, there is no data reusage. Each element of matrix `a` and matrix
`b` is accessed only once.

Under the assumption that the matrices are stored in row-major order, and
because of the principles of data locality, after accessing `a[j][i]` access to
`a[j][i + 1]`, `a[j][i - 1]` and maybe a few other neighbors is very cheap
(since that data is in the data cache). But, access to `a[j][i + 1]` happens
much later after access to `a[j][i]` and doesn't benefit data locality.

Loop tiling can be used to decrease the time between consecutive accesses to
`a[j][i]` and `a[j][i + 1]`. We tile the loop like this:

```c
for (int ii = 0; ii < n; ii += TILE_SIZE) {
  for (int jj = 0; jj < n; jj += TILE_SIZE) {
    for (int i = ii; i < MIN(n, ii + TILE_SIZE); i++) {
      for (int j = jj; j < MIN(n, jj + TILE_SIZE); j++) {
        b[i][j] = a[j][i];
      }
    }
  }
}
```

Instead of accessing data inside one large tile with dimensions `n * n`, the
program is accessing data inside many smaller tiles with dimensions
`TILE_SIZE * TILE_SIZE`. This improves the loop's data locality and makes for a
faster loop. 

### What are the prerequisites for loop tiling?

To perform loop tiling, the loops have to be
[perfectly nested](/Glossary/Perfect-loop-nesting.md) and a certain type of
loops with loop-carried dependencies cannot be tiled. 

Additionally, doing loop tiling only makes sense if there is data reuse or an
inefficient memory access pattern that cannot be fixed with loop interchange.
Without it, there can be no speed improvements.

### How to perform loop tiling?

Loop tiling is typically performed on a loop nest. As an explanation it sounds
quite complicated, but in practice there is a pattern that works most of the
time. Let's take the example:

```c
for (int i = 0; i < n; i++) {
  for (int j = 0; j < n; j++) {
    b[i][j] = a[j][i];
  }
}
```

There are two loops in this loop nest. For each loop we introduce another outer
loop. i.e. for loop over `i` we introduce another loop over `ii` and for loop
over `j` we introduce another loop over `jj`.

The outer loops (over `ii` and `jj`) iterate from 0 to `n` with an increment of
`TILE_SIZE`. The corresponding inner loop (over `i` and `j`) iterates from the
value of the corresponding outer loop index (`ii` for `i` and `jj` for `j`) to
the length of the tile or end of the array. It looks like this:

| Outer loop                                  | Inner loop                                          |
|---------------------------------------------|-----------------------------------------------------|
| `for (int ii = 0; ii < n; ii += TILE_SIZE)` | `for (int i = ii; i < MIN(ii + TILE_SIZE, n); i++)` |
| `for (int jj = 0; jj < n; jj += TILE_SIZE)` | `for (int j = jj; j < MIN(jj + TILE_SIZE, n); j++)` |

We first place outer loops and then inner loops. The resulting code looks like this:

```c
for (int ii = 0; ii < n; ii += TILE_SIZE) {
  for (int jj = 0; jj < n; jj+=TILE_SIZE) {
    for (int i = ii; i < MIN(n, ii +TILE_SIZE); i++) {
      for (int j = jj; j < MIN(n, jj + TILE_SIZE); j++) {
        b[i][j] = a[j][i];
      }
    }
  }
}
```
