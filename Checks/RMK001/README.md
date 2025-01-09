# RMK001: loop nesting that might benefit from hybrid parallelization using multithreading and SIMD

### Issue

Use multi-threading and SIMD hybrid parallelization to maximize the parallel
performance of the nested loop.

### Actions

Parallelize the outer loop using multi-threading and the inner loop using
vectorization.

### Relevance

Certain nested loops may benefit from using a hybrid parallelization approach
where the outer loop is parallelized using multi-threading while using
vectorization for the inner loop. In this way, the use of modern hardware is
maximized by using as many CPU cores as possible through multi-threading and at
the same time using the vectorization hardware available in each core.
