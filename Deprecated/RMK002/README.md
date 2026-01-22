# RMK002: Loop nesting that might benefit from hybrid parallelization using offloading and SIMD

> [!WARNING]
> This check was deprecated in favor of [PWR053](../../Checks/PWR053/README.md),
> [PWR054](../../Checks/PWR054/README.md),
> [PWR055](../../Checks/PWR055/README.md),
> [PWR056](../../Checks/PWR056/README.md), and
> [PWR057](../../Checks/PWR057/README.md). This is because RMK002 combined
> offloading and vectorization concerns into a single check. The highlighted
> checks split these cases into more focused and actionable rules.

### Issue

Use offloading to the GPU and SIMD hybrid parallelization to maximize the
parallel performance of the nested loop.

### Actions

Parallelize the nested loops using offloading for the outer loop and SIMD
vectorization for the inner loop.

### Relevance

Certain nested loops may benefit from using a hybrid parallelization approach by
offloading them to the GPU while requesting vectorization for the inner loop. In
this way, the use of modern hardware is maximized by offloading work to the GPU
and at the same time exploiting the vectorization-like GPU capabilities. For
instance, NVIDIA GPUs organize their processing units in a hierarchical
structure whose smaller blocks are groups of 32 units called *warps* which
execute the same instruction. Every time one of these GPUs execute an
instruction, at least 32 of its processing units are executing it in parallel.
This is analogous to a SIMD processor with 32-element lanes and can be exploited
in the same fashion.
