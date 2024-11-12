# PWR030: Remove pointer assignment preventing performance optimization for perfectly nested loops

### Issue

A pointer assignment statement interleaved between
[two nested loops](../../Glossary/Perfect-loop-nesting.md) prevents the usage of
[loop interchange](../../Glossary/Loop-interchange.md) to improve the memory access
pattern and loop's performance.

### Actions

Remove the pointer assignment instruction between loop headers to make the loops
perfectly nested.

### Relevance

Performance optimization best practices recommend writing programs that read and
write data laid out consecutively in the memory of the computer. However,
programmers must use
[memory access patterns](../../Glossary/Memory-access-pattern.md) that matches the
data layout rules for multi-dimensional arrays (e.g. in C/C++ use a row-wise
access pattern for multi-dimensional static arrays).

Loop interchange is a performance optimization technique frequently used to
enable consecutive memory accesses to multi-dimensional arrays. In the scope of
perfectly nested loops its implementation is straightforward and consists of
interchanging the loop headers only. For non perfectly nested loops it may
require further code rewriting, but it usually pays off the effort because it
brings high performance gains.

### Code example

Consider the following example:

```c
for (int i = 0; i < size; i++) {
  for (int j = 0; j < size; j++) {
    float *a = aa[i];
    float *b = bb[0] + j;
    float *c = &cc[i][j];
    for (int k = 0; k < size; k++) {
      c[0] = c[0] + a[k] * b[0];
      b += size;
    }
  }
}
```

This is a classical matrix multiplication algorithm written using a pointer
based notation. Pointer assignments on lines 3, 4 and 5 prevent
[perfect nesting of the loops](../../Glossary/Perfect-loop-nesting.md) and
[loop interchange](../../Glossary/Loop-interchange.md).

By switching to an index based notation, we can convert the loop nest to a
perfectly nested one and enable loop interchange:

```c
for (int i = 0; i < size; i++) {
  for (int j = 0; j < size; j++) {
    for (int k = 0; k < size; k++) {
      cc[i][j] = cc[i][j] + aa[i][k] * b[k][j];
    }
  }
}
```

### References

* [PWR030 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR030/)
