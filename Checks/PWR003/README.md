# PWR003: Explicitly declare pure functions

### Issue

Mark pure functions as such to allow for optimizations. In computer
programming, a pure function is a function that has the following properties:

1. Its only side effect is returning a value. It does not modify any memory
except for local variables.

2. Its return value is the same for the same arguments. It does not depend on
any system resource that may change between calls.

Some programming languages provide built-in keywords to declare properties
about functions, such as Fortran `pure` keyword. Others need extensions
implemented by compilers/tools, for instance the GNU C compiler `const`
attribute (note that it also has a `pure` attribute not compliant with the
definition presented here).

### Actions

Add the appropriate annotations to your code explicitly. For instance:

* In the GCC compiler for the C programming language, use the keyword
`__attribute__((const))`.

* In the Fortran programming language, use the built-in keyword `pure`.

### Relevance

Explicitly declaring properties about functions called in the code provides
valuable hints that can enable optimizations or simplify analysis. Determining
whether a function is pure can be costly for a developer and even infeasible
for automated analysis tools.

One of the biggest challenges of parallel programming is to do data scoping
correctly and decide on how to manage all the variables of the code properly.
Data flow analysis can be simplified through the identification of pure
functions, which are free of side effects. Thus, declaring functions as pure
states that invoking those functions won't introduce race conditions, which
facilitates analysis both to developers and compilers/tools.

### Code example

The example C code shown below has a function `example` with calls to two
functions. On the one hand, `example_pure` is a pure function because it is
free of side effects other than the returned value. On the other hand,
`example_impure` is not pure because it performs write operations on the memory
locations written through a pointer-type argument of the function, which means
that it has side effects.

```c
#ifdef __GNUC__
  #define PURE __attribute__((const))
#else
  #define PURE
#endif

PURE int example_pure(int a, int b) {
  return a + b;
}

int example_impure(int a, int *b) {
  *b = a + 1;
  return a + *b;
}

void example() {
  int result[10];
  int b = 1;

  for (int i = 0; i < 10; i++) {
    result[i] = example_pure(i, b); // No side effects
  }

  for (int i = 0; i < 10; i++) {
    result[i] = example_impure(i, &b); // Side effects on variable b
  }
}
```

### Related resources

* [PWR003 examples](../PWR003)

### References

* [Pure function](https://en.wikipedia.org/wiki/Pure_function)

* [Side effects of function
  calls](https://en.wikipedia.org/wiki/Side_effect_(computer_science))

* [GNU Manual: 6.30 Declaring Attributes of
Functions](https://gcc.gnu.org/onlinedocs/gcc-8.1.0/gcc/Common-Function-Attributes.html#Common-Function-Attributes)
[last checked May 2019]

* [XL Fortran for Linux, V14.1 - Language Reference: Pure
  procedures](https://www.ibm.com/support/knowledgecenter/SSAT4T_14.1.0/com.ibm.xlf141.linux.doc/language_ref/pure.html)

* [PURE | Intel® Fortran Compiler 19.0 Developer Guide and
  Reference](https://software.intel.com/en-us/fortran-compiler-developer-guide-and-reference-pure)
