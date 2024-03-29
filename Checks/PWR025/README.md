# PWR025: Consider annotating pure function with OpenMP `declare simd`

### Issue

A SIMD version of the function can most likely be generated by the compiler.

### Actions

Annotate the pure function with `#pragma omp declare simd`.

### Relevance

A loop invoking functions tends to make it difficult for the compiler to
[vectorize](../../Glossary/Vectorization.md). However, calls to some functions can be
vectorized. Calls to a pure function (function whose return value depends only
on function parameters) can be vectorized if the compiler is instructed to do so
with a compiler pragma.

>**Notes**  
>If the compiler manages to inline the function, then vectorization pragma is
>not needed. To see the performance benefit of this approach, the caller loop
>and called functions must reside in different compilation units.

Also, make sure OpenMP support in your compiler is enabled using compiler
switches (typically `-fopenmp-simd` or `-fopenmp`).

### Code example

The following loop invokes a pure function `foo`:

```c
int foo(int a) {
  return 2 * a;
}

void example(int *A, int n) {
  for (int i = 0; i < n; i++) {
    A[i] = foo(i);
  }
}
```

By adding the `#pragma omp declare simd` the compiler will create a vectorizable
version of `foo`:

```c
#pragma omp declare simd
int foo(int a) {
  return 2 * a;
}

void example(int *A, int n) {
  for (int i = 0; i < n; i++) {
    A[i] = foo(i);
  }
}
```

### References

* [SIMD Directives - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu42.html#x65-1390002.9.3)
