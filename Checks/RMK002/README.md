# RMK002: Loop nesting that might benefit from hybrid parallelization using offloading and SIMD

### Issue

Use offloading to the GPU and SIMD hybrid parallelization to maximize the
parallel performance of the nested loop.

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

### Actions

Parallelize the nested loops using offloading for the outer loop and SIMD
vectorization for the inner loop. You can do so for OpenMP by invoking:

```bash
pwdirectives --offload omp-teams+simd foo.c:5 -o foo-hybrid.c
```
