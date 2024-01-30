# PWR022: Move invariant conditional out of the loop to facilitate vectorization

### Issue

Conditional evaluates to the same value for all loop iterations and can be
[moved outside of the loop](/Glossary/Loop-unswitching.md) to favor
[vectorization](/Glossary/Vectorization.md).

### Actions

Move the invariant conditional outside of the loop by duplicating the loop body.

### Relevance

Classical vectorization requirements do not allow branching inside the loop
body, which would mean no `if` and `switch` statements inside the loop body are
allowed. However, loop invariant conditionals can be extracted outside of the
loop to facilitate vectorization. Therefore, it is often good to extract
invariant conditional statements out of vectorizable loops to increase
performance. A conditional whose expression evaluates to the same value for all
iterations (i.e. is a loop-invariant) can be safely moved outside the loop since
it will always be either true or false.

>**Note**  
>This optimization is called [loop unswitching](/Glossary/Loop-unswitching.md)
>and the compilers can do it automatically in simple cases. However, in more
>complex cases, the compiler will omit this optimization and therefore it is
>beneficial to do it manually.

### Code example

The following loop contains a condition that is invariant for all its
iterations. Not only may this introduce an unnecessary redundant comparison, it
may also make the vectorization of the loop more difficult for some compilers.

```c
int example(int *A, int n) {
  int total = 0;
  for (int i = 0; i < n; i++) {
    if (n < 10) {
      total++;
    }
    A[i] = total;
  }
  return total;
}
```

The loop invariant can be extracted out of the loop in a simple way, by
duplicating the loop body and removing the condition. The resulting code is as
follows:

```c
int example(int *A, int n) {
  int total = 0;
  if (n < 10) {
    for (int i = 0; i < n; i++) {
      A[i] = ++total;
    }
  } else {
    for (int i = 0; i < n; i++) {
      A[i] = total;
    }
  }
  return total;
}
```

### Related resources

* [PWR022 examples at GitHub](/Checks/PWR022)

### References

* [Loop unswitching](/Glossary/Loop-unswitching.md)

* [Vectorization](/Glossary/Vectorization.md)
