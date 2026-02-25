# PWR086: Prefer array-based notation over pointer-based notation for readability

### Issue

Using pointer increments or pointer assignments to access array elements reduces
code readability and is more error-prone compared to equivalent array-based
(index-based) notation.

### Actions

Replace pointer-based array access patterns with array-based (index-based)
notation.

### Relevance

Pointer-based notation for accessing array elements is a common pattern in C and
C++ code. While functionally equivalent to array-based notation, pointer-based
access introduces unnecessary complexity that harms code quality:

- **Error-proneness**: pointer arithmetic is a frequent source of off-by-one
  errors and out-of-bounds accesses, especially when accessing multi-dimensional
  arrays with a single pointer `*`, where offsets must be manually computed by
  multiplying indices with the corresponding dimensions. Array-based notation
  makes the intent explicit and reduces the chance of such mistakes.
- **Readability**: understanding which element of the array is being accessed
  requires mentally tracking the pointer's value across modifications in the
  control flow. With array-based notation, the accessed element is immediately
  clear from the index expression.
- **Maintainability**: changing access regions is significantly harder when
  pointer arithmetic must be updated in tandem. Array-based notation keeps the
  indexing logic self-contained and localized.

### Code example

The following loop uses a pointer increment to walk through the elements of
array `b`:

```c
void example(float *a, float *b, float *c, unsigned size, unsigned inc) {
  float *bTemp1 = b;
  for (unsigned i = 0; i < size; i++) {
    c[0] += (a[i] * bTemp1[0]);
    bTemp1 -= inc;
  }
}
```

The pointer `bTemp1` is decremented by `inc` on every iteration, making it
difficult to see at a glance which element of `b` is accessed in each iteration.
Replacing the pointer notation with array indexing makes the access pattern
explicit:

```c
void solution(float *a, float *b, float *c, unsigned size, unsigned inc) {
  for (unsigned i = 0; i < size; i++) {
    c[0] += (a[i] * b[(int)(-i * inc));
  }
}
```

Now the relationship between the loop variable `i` and the accessed element of
`b` is immediately visible.

> [!TIP]
> Array-based notation also facilitates compiler optimizations such as automatic
> vectorization and loop interchange. See [PWR028](../PWR028/README.md) and
> [PWR030](../PWR030/README.md) for performance-focused checks covering similar
> code patterns.

### Related resources

* [PWR086 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR086/)
* [PWR028: Remove pointer increment preventing performance optimization](../PWR028/README.md)
* [PWR030: Remove pointer assignment preventing performance optimization for perfectly nested loops](../PWR030/README.md)

### References

* [CWE-468: Incorrect Pointer Scaling](https://cwe.mitre.org/data/definitions/468.html)
