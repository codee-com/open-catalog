# PWR023: Add `restrict` for pointer function parameters to hint the compiler that vectorization is safe

### Issue

Use `restrict` for pointer function parameters to hint the compiler that there
is no [pointer aliasing](/Glossary/Pointer-aliasing.md) preventing
[vectorization](/Glossary/Vectorization.md).

### Actions

Verify that there is no aliasing at the call sites and add `restrict` to the
pointer function parameters.

### Relevance

Compilers can automatically vectorize only those loops for which they can
guarantee that the pointers do not alias one another, i.e. no memory address is
accessible through two different pointers. The developer can use the `restrict`
C keyword to inform the compiler that the specified block of memory is not
aliased by any other block. Providing this information can help the compiler
generate more efficient code or vectorize the loop. Therefore it is always
recommended to use `restrict` whenever possible so that the compiler has as much
information as possible to perform optimizations such as vectorization.

>**Notes**<br>
>The compilers can emit runtime checks, however, in the presence of too many
>pointers, this is not possible.

### Code example

The following loop exhibits potential pointer aliasing between pointers `x` and
`y`:

```c
int example(int *x, int *y) {
  int sum = 0;
  for (int i = 0; i < 10; ++i) {
    sum += x[i];
    y[i] = 2;
  }
  return sum;
}
```

If you are sure that no aliasing exists between the arguments, adding the
`restrict` keyword will hint the compiler so that it can proceed with
optimizations such as vectorization:

```c
int example(int *restrict x, int *restrict y) {
  int sum = 0;
  for (int i = 0; i < 10; ++i) {
    sum += x[i];
    y[i] = 2;
  }
  return sum;
}
```
