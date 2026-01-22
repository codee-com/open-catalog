# RMK007: Vectorization opportunity within a multithreaded region

> [!WARNING]
> This check was deprecated in favor of [PWR050](../../Checks/PWR050/README.md),
> [PWR051](../../Checks/PWR051/README.md),
> [PWR052](../../Checks/PWR052/README.md),
> [PWR053](../../Checks/PWR053/README.md), and
> [PWR054](../../Checks/PWR054/README.md). This is because RMK007 combined
> multithreading and vectorization concerns into a single check. The highlighted
> checks split these cases into more focused and actionable rules.

### Issue

There is a SIMD vectorization opportunity within a multithreaded region.

### Actions

Add vectorization directives to instruct the compiler to vectorize the loop.

### Relevance

SIMD vectorization is performed at the lowest level in hardware and is usually
compatible with higher forms of parallelization. In this case, it could
potentially be used to further increase the performance of a multithreaded
computation.
