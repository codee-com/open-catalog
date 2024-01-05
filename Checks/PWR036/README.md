# PWR036: Avoid indirect array access to improve performance

### Issue

[Indirect array access](/Glossary/Memory-access-pattern.md) may impact
performance.

### Actions

Consider using techniques like loop fusion,
[loop interchange](/Glossary/Loop-interchange.md),
[loop tiling](/Glossary/Loop-tiling.md) or changing the data layout to avoid
non-consecutive accesses in hot loops.

### Relevance

Accessing an array indirectly (e.g. through another array containing the
positions to be accessed) is generally less efficient than accessing consecutive
positions because the latter improves
[locality of reference](/Glossary/Locality-of-reference.md).

### Code example

Consider the example code below to illustrate the presence of indirect access
patterns. The elements of array `a` are accessed in an indirect manner through
the array b. Thus, the code exhibits random accesses that cannot be predicted
before the actual execution of the code.

```c
void example(float *a, unsigned *b, unsigned size) {
  for (unsigned i = 0; i < size; ++i) {
    a[b[i]] = 0.0f;
  }
}
```

Next, consider another example code where memory access patterns are optimized
in order to improve locality of reference. More specifically, the elements of
array `a` are accessed indirectly, through array `index`. What this means is
that the program is accessing random elements of the array `a`, which leads to a
low performance because of the poor usage of the memory subsystem.

```c
for (int i = 0; i < LEN_1D; ++i) {
  for (int j = 1; j < LEN_1D; j++) {
    c[i] += a[index[j]];
  }
}
```

The alternative implementation shown below takes advantage of loop interchange
to improve locality of reference. Now, the loop over `j` becomes the outer loop,
and the loop over `i` becomes the inner loop. By doing this, the access to
`a[index[j]]` is an access to a constant memory location, since the value of `j`
doesn't change inside the loop. This leads to performance improvement.

```c
for (int j = 1; j < LEN_1D; j++) {
  for (int i = 0; i < LEN_1D; ++i) {
    c[i] += a[index[j]];
  }
}
```

### Related resources

* [PWR036 examples at GitHub](/Checks/PWR036)

### References

* [Memory access pattern](/Glossary/Memory-access-pattern.md) (indirect array access)

* [Locality of reference](/Glossary/Locality-of-reference.md)

* [Loop interchange](/Glossary/Loop-interchange.md)

* [Loop tiling](/Glossary/Loop-tiling.md)
