# RMK007: Vectorization opportunity within a multithreaded region

### Issue

There is a SIMD vectorization opportunity within a multithreaded region.

### Actions

Add vectorization directives to instruct the compiler to vectorize the loop.

### Relevance

SIMD vectorization is performed at the lowest level in hardware and is usually
compatible with higher forms of parallelization. In this case, it could
potentially be used to further increase the performance of a multithreaded
computation.
