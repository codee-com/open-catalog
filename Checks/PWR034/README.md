# PWR034: avoid strided array access to improve performance

### Issue

Strided array access may impact performance.

### Actions

Consider using techniques like loop fusion,
[loop interchange](/Glossary/Loop-interchange.md),
[loop tiling](/Glossary/Loop-tiling.md) or changing the data layout to avoid
non-sequential access in hot loops.

### Relevance

Accessing an array using a non-unit stride is less efficient than accessing
consecutive positions because the latter improves the
[locality of reference](/Glossary/Locality-of-reference.md). Note that C/C++
[column-wise matrix access](/Glossary/Row-major-and-column-major-order.md)
is an example non-unit stride, where the stride is the column width.

### Code example

The following code shows a loop with a strided access to array `a` with stride
`2`. Avoiding it would require changing the data layout of the program, in
general.

```c
void example(float *a, unsigned size) {
  for (unsigned i = 0; i < size; i += 2) {
    a[i] = 0.0f;
  }
}
```

Another code with strided accesses is show below. In this case, both variables
`a` and `b` have a stride `LEN`.

```c
for (int i = 0; i < LEN; ++i) {
  for (int j = 1; j < LEN; j++) {
    a[j * LEN + i] = a[j * LEN + i - 1] + b[j * LEN + i];
  }
}
```

Note that by using loop interchange, the loop order changes from `ij` to
`ji`. The resulting code shown below has sequential accesses (i.e. stride
`1`) for variables `ij` and `b` in the scope of the innermost loop. Note in this
case a code change solves the issue, no change in data layout is required.

```c
for (int j = 1; j < LEN; ++j) {
  for (int i = 0; i < LEN; i++) {
    a[j * LEN + i] = a[j * LEN + i - 1] + b[j * LEN + i];
  }
}
```

### Related resources

* [PWR034 examples at GitHub](/Checks/PWR034)

### References

* [Loop interchange](/Glossary/Loop-interchange.md)

* [Memory access pattern](/Glossary/Memory-access-pattern.md) (strided access pattern)
