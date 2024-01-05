# Memory access pattern

**Memory access pattern** represents the pattern in which a piece of code or a
loop accesses its data. In most modern computer systems the bottleneck is the
memory subsystem, and the memory access pattern can have a significant impact on
loop performance.

It was noticed that programs often exhibit
[locality of reference](/Glossary/Locality-of-reference.md) when accessing data,
and the memory subsystem is built to exploit this fact. However, not all
programs are equally efficient regarding the way they use the memory subsystem,
and there are code transformations that can help improve the memory access
pattern for better performance.

There are four principal memory access patterns in programs:

* Constant - happens when a loop is accessing the same memory location over and
over.

* Sequential (sometimes called consecutive or linear) - happens when a loop is
accessing data sequentially in memory

* Strided - happens when a loop is accessing data sequentially in memory, but the
difference between the two neighboring memory accesses is greater than 1.

* Random - happens when a loop is accessing data without any observable pattern.

In the case of nested loops, the memory access pattern is always analyzed in the
context of the innermost loop. The statements in the innermost loop execute most
often and they have the largest impact on performance. We observe how the memory
is accessed when the iterator of the innermost loop changes its value.

To illustrate the memory access patterns, consider the following loop nest:

```c
for (int i = 0; i < n; i++) {
  d[i] = 0.0;
  for (int j = 0; j < n; j++) {
    d[i] += a[j] + b[j * n] + c[index[j]];
  }
}
```

This loop is accessing data from the five arrays: `a`, `b`, `c`, `index` and
`d`. The innermost loop iterates over `j`, so the memory access patterns are as
follows:

* Access to `d[i]` is constant. It doesn't depend on the value of `j` and it has
the same value inside the innermost loop.

* Access to `a[j]` is sequential. Everytime the iterator variable `j` increases by
1, the loop is accessing the next neighboring element. The same applies to the
access to `index[j]`.

* Access to `b[j * n]` is strided. Everytime the iterator variable `j` increases
by 1, the loop is accessing the element of the array `b` increased by `n`.

* Access to `c[index[j]]` is random. The value accessed when the iterator variable
`j` increases its value is not known and it is considered random.

### Performance Implication of Memory Access Pattern

As already mentioned, the memory access pattern is very important for the loop's
speed. Here is the list of memory access pattern with regards to performance:

* Constant memory access pattern is the fastest. Typically, the compiler can
allocate a register to replace reads and writes to memory.

* Sequential memory access pattern is the next best. It uses the memory
subsystem in the most optimal way compared to other memory access patterns.

* Strided memory access pattern is somewhere in the middle performance-wise. It
doesn't use the memory subsystem optimally, but the CPU can figure out the
memory access pattern and prefetch data before it is needed.

* Random memory access pattern is the worst.

There are transformations, such as loop interchange, loop tiling, loop
sectioning, etc. where most of the benefit comes from improving the memory
access pattern or converting from the more expensive memory access pattern to a
cheaper one.

When it comes to [vectorization](/Glossary/Vectorization.md), the compilers
typically vectorize loops with constant and sequential memory access patterns.

### Related resources

* [Vectorization](/Glossary/Vectorization.md)

* [Loop interchange](/Glossary/Loop-interchange.md)

* [Loop tiling](/Glossary/Loop-tiling.md)

* [Loop sectioning](/Glossary/Loop-sectioning.md)
