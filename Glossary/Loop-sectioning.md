# Loop sectioning

**Loop sectioning** is a program optimization technique that aims to improve the
efficiency of vectorization by splitting the loop execution into several
sections.

Instead of iterating from `0` to `N`, the loop iterates in sections which are
smaller in size, e.g. `0` to `S`, from `S` to `2S - 1`, etc.

There are two distinct use cases for loop sectioning:

* Enabling [vectorization](Vectorization.md) of `for` loops with
`break` and some uncountable loops. These loops typically cannot be vectorized,
but loop sectioning makes this possible.

* Improving the [locality of reference](Locality-of-reference.md)
after [loop fission](Loop-fission.md). In this case, loop sectioning
can be considered as a special subtype of
[loop tiling](Loop-tiling.md), but the goal of optimization is
different.

### Vectorizing loops with `break` statements in the loop body

To illustrate loop sectioning, let's have a look at the following example:

```c
int index;
for (int i = 0; i < n; i++) {
  if (a[i] == 0) {
    index = i;
    break;
  }
}
```

This is a search loop which searches for the first element of the array `a` with
value 0. These loops typically cannot be vectorized.

By applying loop sectioning, we get the following loop:

```c
#define SECTION_SIZE 8

int index;
for (int ii = 0; ii < n; ii += SECTION_SIZE) {
  int found_count = 0;
  for (int i = ii; i < (ii + SECTION_SIZE); i++) {
    if (a[i] == 0) {
      found_count++;
    }
  }

  if (found_count > 0) {
    for (int i = ii; i < (ii + SECTION_SIZE); i++) {
      if (a[i] == 0) {
        index = i;
        goto exit;
      }
    }
  }
}

exit:
// ...
```

In the original loop, we process values in batches of 1: one batch equals one
element. After loop sectioning, we process values in batches of `SECTION_SIZE`.
The loop on lines 7-11 just notes that there is an element which has the desired
property (is 0), but doesn't break out of the loop. This loop is vectorizable.

When the element with a desired property is found (line 13), we rerun the loop
again to find the exact element. This loop cannot be vectorized because of the
`goto` statement in the loop body, but it is executed only once and has a short
trip count, so it doesn't matter.

### Mitigating the effect of repeated access to the same data

Take a look at the example loop produced by
[loop fission](Loop-fission.md):

```c
double *tmp = malloc(n * sizeof(double));

for (int i = 0; i < n; i++) {
  tmp[i] = expensive_computation();
}

int j = 0;
for (int i = 0; i < n; i++) {
  if (tmp[i] > 0) {
    b[j] = tmp[i];
    j++;
  }
}

free(tmp);
```

There are two loops, and both of them access the same array `tmp`. The first
loop writes to `tmp` and the second reads from it.

If the size of the array `tmp` is small, then when the first loop finishes, the
values of the array `tmp` will most likely still be in the data cache and the
second loop can process them faster.

If the size of the array `tmp` is however larger than the size of the data
caches, when the first loop has finished, all the data that the second loop
needs has already been evicted from the cache. To remedy this, we can perform
loop sectioning. The loop sectioning looks like this:

```c
#define SECTION_SIZE 512

double *tmp = malloc(n * sizeof(double));
int j = 0;

for (int ii = 0; ii < n; ii += SECTION_SIZE) {
  for (int i = ii; i < MIN(ii + SECTION_SIZE, n); i++) {
    tmp[i] = expensive_computation();
  }

  for (int i = ii; i < MIN(ii + SECTION_SIZE, n); i++) {
    if (tmp[i] > 0) {
      b[j] = tmp[i];
      j++;
    }
  }
}

free(tmp);
```

Instead of running the first loop from 0 to `n` (where `n` can be large), and
then running the second loop from 0 to `n`, the first loop and second loop take
turns in executing: the first loop runs from 0 to `SECTION_SIZE - 1` followed by
the second loop, then the first loop runs from `SECTION_SIZE` to
`2 * SECTION_SIZE - 1`, followed by the second loop, etc.

When running like this, the values of the array tmp are still in the data cache
after the first loop is done, so the second loop can reuse the same data from
the data cache instead of loading it from the memory.
