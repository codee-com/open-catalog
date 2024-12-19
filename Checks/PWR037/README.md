# PWR037: Potential precision loss in call to mathematical function

### Issue

Calling a mathematical function intended for a smaller data type results in
lower-precision operations and potential loss of information.

### Actions

Replace the mathematical function call with the appropriate alternative version
that matches the input data type.

### Relevance

The C programming language does not support function overloading, which would
allow multiple functions to share the same name but handle different argument
types. To address this limitation, the standard library of C provides multiple
versions of mathematical functions, each tailored to specific data types. For
example:

- `sqrtf()` for `float`.
- `sqrt()` for `double`.
- `sqrtl()` for `long double`.

Using a function intended for a smaller data type, such as `sqrtf()` with a
`double` input, causes an implicit narrowing of the input value. This implicit
type conversion is allowed by the standard, but results in a loss of precision
that can propagate through chained calculations, impacting the overall accuracy
of the program.

### Code example

Consider the following code, which demonstrates the loss of precision when
using `sqrtf()` with a `double` input:

```c
// example.c
#include <math.h>
#include <stdio.h>

__attribute__((const)) double square_root(const double value) {
  return sqrtf(value);
}

int main() {
  printf("Square root of 2 is: %0.15f\n", square_root(2.0));
}
```

Compiling and running this code produces an approximation of the square root of
2:

```txt
$ gcc --version    
gcc (Debian 12.2.0-14) 12.2.0
$ gcc example.c -lm -o example
$ ./example
Square root of 2 is: 1.41421 35381 69861
```

Note the error in the result beyond the seventh decimal place. The accurate
approximation for the square root of 2 would be `1.41421 35623 73095`. The
mismatch corresponds to the 7-digit precision limit of the `float` data type on
systems using IEEE-754 floating-point arithmetic, like `x86`.

Although the error might seem small, such inaccuracies can rapidly compound
across chained computations, resulting in significant deviations from the
expected results.

To fix this issue, simply replace the `sqrtf()` call with `sqrt()` to leverage
the `double` precision:

```c
// solution.c
...

__attribute__((const)) double square_root(const double value) {
  return sqrt(value);
}

...
```

Compiling and running the corrected code procedures a more accurate
approximation of the square root of 2:

```txt
$ gcc solution.c -lm -o solution
$ ./solution
Square root of 2 is: 1.41421 35623 73095
```

> [!IMPORTANT]
> In this context, higher-precision calculations are not merely a performance
> overhead. Instead, they address an invalid optimization that compromises the
> accuracy and reliability of the code. If lower-precision calculations are
> genuinely intended for performance reasons, adjust the variable's data type
> to ensure it matches the called function.

### Related resources

- [PWR037
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR037/)

### References

- ["Common mathematical
functions"](https://en.cppreference.com/w/c/numeric/math), cppreference.com.
[last checked December 2024]

- ["Function overloading"](https://en.wikipedia.org/wiki/Function_overloading),
Wikipedia Contributors. [last checked December 2024]

- ["IEEE
754"](https://en.wikipedia.org/wiki/IEEE_754#Basic_and_interchange_formats),
Wikipedia Contributors. [last checked December 2024]
