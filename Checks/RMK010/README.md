# RMK010: The vectorization cost model states the loop is not a SIMD opportunity due to strided memory accesses in the loop body

### Issue

Loop is not an SIMD opportunity because of the strided memory accesses in the
loop body.

### Actions

If the loop is part of the loop nest, consider applying loop interchange or loop
tiling.

### Relevance

[Memory access pattern](../../Glossary/Memory-access-pattern.md) is very important
for good software performance. The loop contains strided memory accesses. This
type of memory access pattern is inefficient from the memory subsystem
perspective, and these loops are typically not good SIMD opportunities.

Loops with strided memory accesses can be vectorized if the strided memory
access is removed. This is typically the case when the strided memory access is
inside a loop nest. In that case, techniques like loop interchange can help
convert the inefficient strided memory access pattern into other, more efficient
access patterns. Techniques like [loop tiling](../../Glossary/Loop-tiling.md) can
also make the dataset smaller and make vectorization beneficial even in the
presence of strides.

Repacking the data to avoid the stride is also one of the ways to remove the
stride. For example, instead of loop which writes to elements of the array
`a[3 * i]`, `a[3 * i + 1]` and `a[3 * i + 2]`, we introduce three separate
arrays and write to them: `a1[i]`, `a2[i]` and `a3[i]`. This kind of
transformation however may require rewriting a large part of the code.
