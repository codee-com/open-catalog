# Loop unswitching

**Loop unswitching** is a program optimization technique, where invariant
conditions inside loops (i.e. conditions whose value is always the same inside
the loop) can be taken outside of the loop by creating copies of the loop.

To illustrate loop unswitching, consider the following example:

```c
for (int i = 0; i < n; i++) {
  if (debug) {
    if (a[i] < 0) {
      log_error("Negative number");
    }
  }
  b[i] = sqrt(a[i]);
}
```

In the above example, we are calculating the `sqrt` of elements of `a[i]`. But
in case `a[i]` is negative and we are debugging, we want to log an error.

The condition `if (debug)` is loop invariant, since the variable `debug` never
changes its value. By doing loop unswitching and moving this condition outside
of the loop, the loop becomes faster. Here is the same loop after loop
unswitching:

```c
if (debug) {
  for (int i = 0; i < n; i++) {
    if (a[i] < 0) {
      log_error("Negative number");
    }
    b[i] = sqrt(a[i]);
  }
} else {
  for (int i = 0; i < n; i++) {
    b[i] = sqrt(a[i]);
  }
}
```

The loop switching was done by replicating the original loop's body, once for
the case where `debug` is true, and another time for the case when `debug` is
false.

Compilers can do loop unswitching automatically, under following conditions:

* The condition is loop invariant: although seemingly trivial, in the presence of
[pointer aliasing](/Glossary/Pointer-aliasing.md), the compiler can omit this
optimization.

* There is a limited number of conditions to unswitch: for each condition value,
the loop body needs to be duplicated. This can lead to an explosion of code
size. If the code grows too large, the compilers can omit this optimization.

In the presence of pointer aliasing or many conditions, it is possible to do
this optimization manually.
