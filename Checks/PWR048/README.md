# PWR048: Replace multiplication/addition combo with an explicit call to fused multiply-add

### Issue

Multiplication/addition combo can result in the compiler emitting two
operations, multiplication and addition, instead of one operation fused
multiply-add (FMA).

### Actions

Replace a combination of multiplication and addition `a + b * c`, with a call to
the `fma` function.

### Relevance

Modern hardware often provides a fused multiply-add (FMA) instruction that
performs multiplication and addition in a single instruction. Compilers, with an
ISA supporting FMA instruction, will fuse independent multiplications and
additions into a single FMA operation.

Most compilers do this automatically when proper optimization flags are
provided. But, if the compiler is configured to work with strict IEEE 754
compliance, then FMA instructions will not be emitted automatically. In that
case, the developer has an option to explicitly use FMA instruction through a
function `fma` available in `math.h` (or `std::fma` available in `cmath`).

### Code example

Have a look at the following code:

```c
double example(double a, double b, double c) {
  return a + b * c;
}
```

In the above example, the expression `a + b * c` is effectively a FMA operation
and it can be replaced with a call to `fma`:

```c
#include <math.h>

double example(double a, double b, double c) {
  return fma(b, c, a);
}
```

>**Note**  
>The above optimization makes sense under the following conditions:
>
>1. The compiler is configured with strict IEEE 754 compliance
>(`-ffp-contract=off` or `-ffp-contract=on` on GCC and clang);
>
>2. and the underlying ISA supports FMA and the compiler is allowed to use
>FMA instruction either using `-mfma` or `-march=ARCH`, where `ARCH` supports
>FMA instruction.

### Related resources

* [PWR048 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR048/)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
