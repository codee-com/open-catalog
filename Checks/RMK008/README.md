# RMK008: SIMD opportunity within an offloaded region

### Issue

There is a SIMD vectorization opportunity within an offloaded region.

### Actions

Add vectorization directives to instruct the compiler to vectorize the loop.

### Relevance

SIMD vectorization is performed at the lowest level in hardware and is usually
compatible with higher forms of parallelization. In this case, it could
potentially be used to further increase the performance of computation offloaded
to accelerator devices such as GPUs.
