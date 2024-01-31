# PWR031: Replace call to pow by multiplication, division and/or square root

### Issue

The `pow` mathematical function is computationally expensive and in many cases
can be replaced by
[faster mathematical operations](../../Glossary/Strength-reduction.md).

### Actions

Replace the `pow` invocation by the corresponding calculation involving
multiplications, divisions and/or square roots.

### Relevance

Function `pow` is commonly used in scientific computations. In general, it is an
expensive function. However, in some cases when the value of exponent is known
at compile time, its runtime can be greatly reduced by replacing it with a
combination of multiplications, divisions and square roots.

>**Note**  
>Some compilers under some circumstances (e.g. relaxed IEEE 754 semantics) can
>do this optimization automatically. However, doing it manually will guarantee
>best performance across all the compilers.

### Code example

The following code invokes `pow` to calculate `x` to the power of `1.5`:

```c
#include <math.h>

void example(float *a, float x) {
  for (int i = 0; i < 10; ++i) {
    a[i] = pow(x, 1.5);
  }
}
```

This can also be accomplished by multiplying `x` by its square root, which is
faster:

```c
#include <math.h>

void example(float *a, float x) {
  for (int i = 0; i < 10; ++i) {
    a[i] = x * sqrt(x);
  }
}
```

### Related resources

* [PWR031 examples](../PWR031)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
