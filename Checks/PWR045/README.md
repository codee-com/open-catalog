# PWR045: Replace division with a multiplication with a reciprocal

### Issue

Divisions are much more expensive than multiplications on modern hardware.
Replacing a loop-invariant division with a multiplication often results in speed
boost.

### Actions

Calculate the reciprocal outside of the loop and replace the division with
multiplication with a reciprocal

### Relevance

If a hot loop involves a division with a loop-invariant constant, instead of
performing the division in each iteration of the loop, one could do the
following:

* For the expression  `A / B`, calculate the reciprocal of the denominator
(`RECIP_B = 1.0 / B`) and put it outside of the loop.

* Replace the expression `A / B`, use `A * RECIP_B`.

### Code example

Have a look at the following code:

```c
float calc_div_recip(float *out, float *in, int n, float b) {
  for (int i = 0; i < n; ++i) {
    out[i] = in[i] / b;
  }
}
```

In the above example, the denominator of division `in[i] / b` is loop-invariant,
i.e. its value doesn't change while the loop is running. Therefore, we can
calculate the reciprocal of `b` and replace the division with the multiplication
by the reciprocal:

```c
float calc_div_recip(float *out, float *in, int n, float b) {
  float recip_b = 1.0f / b;
  for (int i = 0; i < n; ++i) {
    out[i] = in[i] * recip_b;
  }
}
```

> [!NOTE]
> This optimization can result in a small change of the result compared to
> original code. Many compilers support relaxed IEEE 754 floating point
> optimization (e.g. `-ffast-math`). If relaxed IEEE 754 mode is enabled with the
> compiler, the compiler might do this optimization automatically. Implementing
> it in the source code might not bring the expected speedup, but it makes the
> optimization explicit in the code.

### Related resources

* [PWR045 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR045/)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
