# Pointer aliasing

Pointer aliasing happens when a single memory location is accessible through
different symbolic names in the programs. The name **pointer aliasing** applies
specifically to accessing the memory location through pointers, although the
same effect can be achieved when accessing the memory location through
references, or any other way.

Pointer aliasing is notorious for breaking compiler optimizations and
introducing hidden loop dependencies. Consider the following example:

```c
void foo(double *a, double *b, int n) {
  for (int i = 0; i < n; i++) {
    b[i] += a[i];
  }
}
```

In case pointers `a` and `b` point to distinct memory blocks, this code is
perfectly [parallelizable](Parallelization.md). But, consider that
someone called the function with the following arguments:

```c
foo(a, a + 1, n - 1);
```

If this is the case, pointers `a` and `b` alias and the loop cannot be
parallelized because of the
[loop-carried dependency](Loop-carried-dependencies.md).
