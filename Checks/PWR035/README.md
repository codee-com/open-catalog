# PWR035: avoid non-consecutive array access to improve performance

### Issue

[Non-consecutive array access](../../Glossary/Memory-access-pattern.md) may impact
performance.

### Actions

Consider using techniques like loop fusion,
[loop interchange](../../Glossary/Loop-interchange.md),
[loop tiling](../../Glossary/Loop-tiling.md)
or changing the data layout to avoid non-consecutive access in hot loops.

### Relevance

Accessing an array in a non-consecutive order is less efficient than accessing
consecutive positions because the latter maximises
[locality of reference](../../Glossary/Locality-of-reference.md).

### Code example

Consider the example code below to illustrate the presence of non-consecutive
access patterns. The elements of array `a` are accessed in a non-consecutive
manner. In the scope of the outer loop `for_i`, all the iterations access the
first row of the array. Thus, the code exhibits repeated accesses to all the
elements of the first row, a total number of times equal to `rows`.

```c
void example(float **a, unsigned rows, unsigned cols) {
  for (unsigned i = 0; i < rows; ++i) {
    for (unsigned j = 0; j < cols; ++j) {
      a[0][j] = 0.0f;
    }
  }
}
```

### Related resources

* [PWR035 examples](../PWR035)

### References

* [Memory access pattern](../../Glossary/Memory-access-pattern.md) (non-consecutive array access)

* [Locality of reference](../../Glossary/Locality-of-reference.md)

* [Loop interchange](../../Glossary/Loop-interchange.md)

* [Loop tiling](../../Glossary/Loop-tiling.md)
