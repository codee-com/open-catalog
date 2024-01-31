# PWR044: Avoid unnecessary floating-point data conversions involving constants

### Issue

Mixing float type value with double type constants results in unnecessary data
conversions.

### Actions

Instead of the double constant, explicitly specify a float constant by appending
the constant with `f`.

### Relevance

According to the C/C++ standard, binary operators (`+`, `-`, `*`, `/`, `%`, `<`,
`>`, `<=`, `>=`, `==`, `!=`) involving a float value and a double constant can
often result in unnecessary data conversions: the float value is first promoted
to a double, then the binary operator is evaluated. Most of the time this is not
the intended behavior.

### Code example

Have a look at the following code:

```c
float example(float a) {
  return a * 2.2;
}
```

During the evaluation of the expression `a * 2.2`, the following happens:

* First, the floating point in variable `a` is promoted from float to double;

* Second, multiplication is performed using doubles;

* And third, the resulting value is converted back to float.

In this particular example, there are two unnecessary conversions: one is
promoting `a` to double, and the other is promoting the result of multiplication
`a * 2.2` back to float. The solution is to use a float constant `2.2f` instead
of the double constant `2.2`.

```c
float example(float a) {
  return a * 2.2f;
}
```

### Related resources

* [PWR044 examples](../PWR044)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
