# RMK014: The vectorization cost model states the loop is not a SIMD opportunity due to unpredictable memory accesses in the loop body

### Issue

Loop is not a SIMD opportunity because of the unpredictable memory accesses in
the loop body.

### Actions

Perform [loop fission](../../Glossary/Loop-fission.md) to isolate the
unpredictable memory accesses to a separate loop. If not possible, consider
evaluating the performance of the alternative scenarios described below.

### Relevance

[Memory access pattern](../../Glossary/Memory-access-pattern.md) is very important
for good software performance. The loop contains unpredictable memory accesses
(either because of dereferencing a pointer or because it accesses array elements
indirectly). This type of memory access pattern is very inefficient from the
memory subsystem perspective, and these loops are typically not good SIMD
opportunities.

Occasionally, loops with unpredictable memory access pattern can benefit from
vectorization: if the loop is computationally expensive, using
[loop fission](../../Glossary/Loop-fission.md) to isolate the unpredictable accesses
to a separate loop can help vectorize the remaining part of the loop.
Alternatively, if the dataset the loop is processing is really small, the loop
can benefit from explicit vectorization using compiler pragmas. Another idea:
if the loop in question is a part of a loop nest, performing
[loop interchange](../../Glossary/Loop-interchange.md) or
[loop tiling](../../Glossary/Loop-tiling.md) can decrease the pressure on the memory
subsystem as well.

### Related resources

* [PWR020: Consider loop fission to enable vectorization](../PWR020/README.md)

* [PWR021: Consider loop fission with scalar to vector promotion to enable vectorization](../PWR021/README.md)

* [PWR022: Move invariant conditional out of the loop to facilitate vectorization](../PWR022/README.md)
