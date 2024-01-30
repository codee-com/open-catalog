# PWR028: Remove pointer increment preventing performance optimization

### Issue

A loop contains a pointer increment statement enabling access to the elements of
an array, but the pointer increment introduces dependencies between the
iterations of the loop that defeat performance optimization.

### Actions

Remove the pointer increment instruction from the loop body, updating the
dereferences of the pointer in the loop body and recalculating the offsets
correspondingly.

### Relevance

Performance optimization best practices give recommendations regarding the
efficient [memory access pattern](/Glossary/Memory-access-pattern.md), but do
not give recommendations regarding a coding style that favors performance
optimizations for the compiler.

Programmers may write programs where the array references are driven by the
pointer variable being incremented in the loop body. From a memory access
pattern perspective, both ways of coding lead to accessing the same data in the
same order at run-time. However, there is a fundamental difference from the
point of view of the instructions executed by the processor: using another
pointer variable incremented in the loop body introduces dependencies between
the iterations of the loop.

Programmers may write different programs, and tools are expected to detect these
challenging situations, help programmers to understand the implications from the
performance viewpoint, and suggest how to overcome these issues through
alternative ways of coding that are more hardware-friendly.

### Code example

The following loop uses pointer increment notation to access the elements of
array `bTemp1`.

```c
void example(float *a, float *b, float *c, unsigned size, unsigned inc) {
  float *bTemp1 = b;
  for (unsigned i = 0; i < size; i++) {
    c[0] += (a[i] * bTemp1[0]);
    bTemp1 -= inc;
  }
}
```

We can get rid of pointer based notation and replace it with array based
notation like this:

```c
void example(float *a, float *b, float *c, unsigned size, unsigned inc) {
  for (unsigned i = 0; i < size; i++) {
    c[0] += (a[i] * bTemp1[-i * inc]);
  }
}
```

### Related resources

* [PWR028 examples at GitHub](/Checks/PWR028)
