# RMK013: Low trip count unknown at compile time may prevent vectorization of the loop

### Issue

Loops with low trip count unknown at compile time do not benefit from
vectorization.

### Actions

There are several ways to fix this:

* Use techniques like loop interchange to increase the loop trip count of the
innermost loop (read more [here](../PWR019/README.md)).

* Disable vectorization of the loop. The portable way to do it is though
`#pragma omp simd if(simd:0)`, however, this is a recent extension of OpenMP and
is not available in older versions of the compilers. Many compilers have
non-portable pragmas to disable vectorization (e.g. clang's
`#pragma clang loop vectorize(disable)`).

* If the loop trip count is unknown at compile time, but it can have only a few
fixed values (e.g. 3, 4 and 5), create a separate version of the loop for each
of the different trip counts.

### Relevance

The main assumption of [vectorization](../../Glossary/Vectorization.md) is that the
same operation is applied to a vector of data instead of to a single piece of
data. The size of the vector is fixed, and it is often the case that the size of
the data set is not an exact multiple of the vector size. In this case, in
addition to a vectorized loop, it is necessary to introduce a drain loop which
processes a few last pieces of data which do not fit the whole vector.

When the loop trip count is very low, it might happen that the vector part of
the loop doesn't run at all, or it runs with only one or two iterations. If this
is the case, the resulting vectorized loop will be the same speed or slower than
its non-vectorized counterpart.

Please note that this recommendation applies only to those loops where the loop
trip count is both low and unknown at compile time. Loops whose trip count is
known at compile time do not have this issue.

### Related resources

* [PWR019: Consider interchanging loops to favor vectorization by maximizing inner loop's trip count](../PWR019/README.md)
