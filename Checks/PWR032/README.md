# PWR032: Avoid calls to mathematical functions with higher precision than required

### Issue

Not calling the most suitable mathematical function for a given data type
results in unnecessary data type conversions and higher precision operations,
which are slower.

### Actions

Replace the mathematical function call with its alternative version matching the
data type.

### Relevance

In C, there are several versions of the same mathematical function for different
types. For example, the square root function is available for floats, doubles
and long doubles through `sqrtf`, `sqrt` and `sqrtl`, respectively. Oftentimes,
the developer who is not careful will not use the function matching the data
type. For instance, most developers will just use "sqrt" for any data type,
instead of using `sqrtf` when the argument is float.

The type mismatch does not cause a compiler error because of the implicit type
conversions. However, this practice will result in suboptimal code due to
expensive calculations with unnecessary high precision and data type
conversions.

### Code example

The following code uses the `sin` function to calculate the sine of the float
variable `x` and store the result in the variable `res`:

```c
#include <math.h>

void example(float x) {
  float res = sin(x);
}
```

This causes a conversion of `x` to a double that is passed to function `sin` and
another of the result which has type double into a float to be stored into the
variable `res`. These two conversion operations can be prevented by using the
most appropriate function `sinf` which matches the float data type:

```c
#include <math.h>

void example(float x) {
  float res = sinf(x);
}
```

### Related resources

* [PWR032 examples](../PWR032)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
