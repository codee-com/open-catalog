# Scalar to vector promotion

Scalar to vector promotion is an optimization technique used to enable further
optimization techniques, notably
[loop interchange](Loop-interchange.md) and
[vectorization](Vectorization.md) through
[loop fission](Loop-fission.md). In this technique, a temporary scalar
is converted to a vector whose value is preserved between loop iterations, with
the goal to enable loop fission needed to extract the statements preventing
optimizations outside of the critical loop.

### Loop interchange

In the case of loop interchange, scalar to vector promotion is performed to
extract the statements blocking
[perfect loop nesting](Perfect-loop-nesting.md). For example:

```c
for (int i = 0; i < n; i++) {
  double sum = 0.0;
  for (int j = 0; j < n; j++) {
    sum += a[j][i];
  }
  b[i] = 0.1 * sum;
}
```

The statements on line 2 and 6 are preventing perfect loop nesting which in turn
prevent loop interchange. To enable loop interchange, we need to get rid of
those two statements. We can do this by promoting scalar `sum` to a vector, like
this:

```c
for (int i = 0; i < n; i++) {
  s[i] = 0.0;
  for (int j = 0; j < n; j++) {
    s[i] += a[j][i];
  }
  b[i] = 0.1 * s[i];
}
```

Then it is relatively easy to extract the statements on line 2 and line 6 to a
dedicated loop and enable loop interchange:

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

The two loops on lines 4 and 5 are perfectly nested and loop interchange can be
performed.

### Enabling vectorization through loop fission

Scalar to vector promotion can be used to enable vectorization by fissioning
(splitting) the loop into vectorizable and non-vectorizable parts. Consider the
following example:

```c
for (int i = 0; i < n; i++) {
  double s = sqrt(a[i]);
  if (s > 1.0) {
    b[j] = s;
    j++;
  }
}
```

The above loop is unvectorizable because of the
[loop-carried dependencies](Loop-carried-dependencies.md) However, it
can be split into a vectorizable (line 2) and non-vectorizable (line 3-6) parts.

To perform the split, however, we need to maintain the value of variable `s`
across loop iterations. We can do this by promoting the scalar variable `s` to a
vector:

```c
for (int i = 0; i < n; i++) {
  s[i] = sqrt(a[i]);
}
for (int i = 0; i < n; i++) {
  if (s[i] > 1.0) {
    b[j] = s[i];
    j++;
  }
}
```
