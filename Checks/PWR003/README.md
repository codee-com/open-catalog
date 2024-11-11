# PWR003: Explicitly declare pure functions

### Issue

Mark functions that do not produce side effects as pure to enable compiler
optimizations.

In computer programming, a function can be considered pure if **its only side
effect is returning a value, without modifying any memory outside its own local
scope**. Some programming languages provide built-in keywords to declare these
properties, whereas others need compiler extensions.

### Actions

Add the appropriate annotations to the function. For instance:

* The GCC compiler for C provides the extension `__attribute__((pure))`.

> [!TIP]
> GCC's `__attribute__((const))` goes a step further by stating that the
> function's return value depends solely on the values of its input arguments,
> thus not being affected by any system resource that may change between calls;
> e.g., depending on global data, receiving a pointer.

* The Clang compiler for C supports both GCC's extensions.

* The ARM compiler for C supports both GCC's extensions.

> [!WARNING]
> This compiler also provides the `__pure` keyword, but it is equivalent to
> `__attribute__((const))`!

* In the Fortran programming language, use the built-in keyword `pure`.

### Relevance

Explicitly declaring properties about the functions called in the code provides
valuable hints that can ease data flow analysis and optimizations for both the
compiler and the developer. Determining whether a function is pure can be a
challenging and time-consuming task, especially in complex codebases.

For that matter, one of the biggest challenges of parallel programming is the
correct management of data scoping; i.e., how to handle all the variables in
the code effectively, to avoid common pitfalls like race conditions. Insights
such as annotating pure functions can be incredibly helpful in this context.

### Code example

#### C

The C code shown below demonstrates `const`, `pure`, and "normal" functions:

```c
// Depends only on its arguments
// No side effects
__attribute__((const)) int example_const(int a, int b) {
  return a + b;
}

int c = 1;

// Depends on external data (c)
// No side effects
__attribute__((pure)) int example_pure(int a) {
  return a + c;
}

// Depends on external data (c)
// Modifies external data (c)
int example_impure(int a) {
  c += 1;
  return a + c;
}
```

* `const` function:
  * Depends only on `a` and `b`. If successive calls are made with the same `a`
    and `b` values, the output will not change.
  * Returns a value without modifying any data outside of the function.

* `pure` function:
  * Depends on `c`, a global variable whose value can be modified between
    successive calls to the function by other parts of the program. Even if
    successive calls are made with the same `a` value, the output can differ
    depending on the state of `c`.
  * Returns a value without modifying any data outside of the function.

* "Normal" function:
  * Depends on `c`, a global variable. This restricts the function to be
    `pure`, at most.
  * However, the function also modifies `c`, memory outside of its scope, thus
    leading to a "normal" function.

In the case of the `pure` and "normal" functions, it is equivalent that they
access a global variable, or a pointer received as an argument, as it is in
either case memory outside the scope of the function.

#### Fortran

The Fortran code shown below demonstrates `pure` and "normal" functions:

```f90
module example_module
   implicit none
   integer :: c = 1
contains
   ! Depends on external data (c)
   ! No side effects
   pure function example_pure(a) result(res)
      integer, intent(in) :: a
      integer :: res
      res = a + c
   end function example_pure

   ! Depends on external data (c)
   ! Modifies external data (c)
   function example_impure(a) result(res)
      integer, intent(in) :: a
      integer :: res
      c = c + 1
      res = a + c
   end function example_impure
end module example_module
```

* `pure` function:
  * Depends on `c`, a public variable whose value can be modified between
    successive calls to the function by other parts of the program. Even if
    successive calls are made with the same `a` value, the output can be
    different depending on the state of `c`.
  * Returns a value without modifying any data outside of the function.

* "Normal" function:
  * Depends on `c`, a public variable. This restricts the function to be
    `pure`, at most.
  * However, the function also modifies `c`, memory outside of its scope, thus
    leading to a "normal" function.

### Related resources

* [PWR003 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR003/)

### References

* [Pure function](https://en.wikipedia.org/wiki/Pure_function)

* [Side effects of function
  calls](https://en.wikipedia.org/wiki/Side_effect_(computer_science))

* [GCC documentation: pure
  attribute](https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-pure-function-attribute)

* [Clang documentation: pure
  attribute](https://clang.llvm.org/docs/AttributeReference.html#pure)

* [ARM Compiler toolchain documentation: pure
  attribute](https://developer.arm.com/documentation/dui0491/i/Compiler-specific-Features/--attribute----pure---function-attribute)

* [Fortran pure
  procedures](https://en.wikibooks.org/wiki/Fortran/Fortran_procedures_and_functions#Pure_procedures)
