# Loop interchange

**Loop interchange** is a performance optimization technique that is used to
improve the loop's [memory access pattern](/Glossary/Memory-access-pattern.md)
and potentially [enable vectorization](/Glossary/Vectorization.md). Loop
interchange if applied correctly can yield a huge performance improvement.

Loop interchange as an optimization technique is applicable to loop nests. A
loop nest consists of two or more nested loops. After loop interchange, a loop
that is once outer has now become inner, and the loop that was once inner has
become outer.

To illustrate loop interchange, consider the following example:

```c
for (int i = 0; i < n; i++) {
  for (int j = 0; j < n; j++) {
    b[i] += c[j][i];
  }
}
```

If we analyze the [memory access pattern](/Glossary/Memory-access-pattern.md)
for the arrays `b` and `c` in the context of the innermost loop, we can
determine that the access to array `b` is constant, whereas access to the
matrix `c` is strided. By doing loop interchange, we get the following loop
nest:

```c
for (int j = 0; j < n; j++) {
  for (int i = 0; i < n; i++) {
    b[i] += c[j][i];
  }
}
```

Notice that the loop over `j`, that was originally the inner loop, is now the
outer loop. Similarly, the loop over `i` that was originally an outer loop is
now the inner loop.

The memory access pattern for `b[i]` is now sequential (originally it was
constant) and the memory access pattern for `c[j][i]` is now sequential too
(originally it was strided). The memory access pattern allows this loop to be
[vectorized](/Glossary/Vectorization.md).

### Requirements for loop interchange

Loop interchange is possible if the following conditions are fulfilled:

* [The loops are perfectly nested](/Glossary/Perfect-loop-nesting.md): all the
statements are inside the innermost loop. If this is not the case, often it is
possible to make them perfectly nested, see
[Perfect-loop nesting](/Glossary/Perfect-loop-nesting.md) on how to do it.

* The value of the inner loop's iterator variable should be independent of the
value of the outer's loop iterator variable.

* Sometimes loop interchange is not possible in the presence of
[loop-carried dependencies](/Glossary/Loop-carried-dependencies.md).
