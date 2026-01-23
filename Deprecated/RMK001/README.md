# RMK001: Loop nesting that might benefit from hybrid parallelization using multithreading and SIMD

> [!WARNING]
> This check was deprecated in favor of [PWR050](../../Checks/PWR050/README.md),
> [PWR051](../../Checks/PWR051/README.md),
> [PWR052](../../Checks/PWR052/README.md),
> [PWR053](../../Checks/PWR053/README.md), and
> [PWR054](../../Checks/PWR054/README.md). This is because RMK001 combined
> multithreading and vectorization concerns into a single check. The highlighted
> checks split these cases into more focused and actionable rules.

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
