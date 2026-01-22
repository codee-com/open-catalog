# RMK008: Vectorization opportunity within an offloaded region

> [!WARNING]
> This check was deprecated in favor of [PWR053](../../Checks/PWR053/README.md),
> [PWR054](../../Checks/PWR054/README.md),
> [PWR055](../../Checks/PWR055/README.md),
> [PWR056](../../Checks/PWR056/README.md), and
> [PWR057](../../Checks/PWR057/README.md). This is because RMK008 combined
> offloading and vectorization concerns into a single check. The highlighted
> checks split these cases into more focused and actionable rules.

### Issue

There is a SIMD vectorization opportunity within an offloaded region.

### Actions

Add vectorization directives to instruct the compiler to vectorize the loop.

### Relevance

SIMD vectorization is performed at the lowest level in hardware and is usually
compatible with higher forms of parallelization. In this case, it could
potentially be used to further increase the performance of computation offloaded
to accelerator devices such as GPUs.
