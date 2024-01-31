# Loop fission

**Loop fission** is a code transformation technique that works by splitting a
loop into multiple loops over the same index range, each one taking only a part
of the original loop body. The goal is to isolate statements that can take
advantage of compiler and hardware optimizations, separating them from other
statements preventing those optimizations. It is worth exploring the application
of loop fission in codes with the following features:

* **Loop-carried dependencies between statements of the loop** - e.g. when the
data needed in the current iteration of the loop depends on the data from the
previous iteration of the loop.

* **Conditional statements in the loop body that depend on the data** - e.g.
when conditional statements in the loop body depend on input data, the execution
path of the code cannot be anticipated and performance optimizations may be
disabled at run-time.

* **Non-sequential memory accesses in the loop** - e.g. non-sequential memory
accesses do not exhibit good locality of reference, thus the code does not take
full advantage of the hardware cache memory and compiler optimizations may be
disabled.

Loop fission enables more efficient code by taking advantage of
[memory efficiency](Locality-of-reference.md),
[vectorization](Vectorization.md) and
[offloading](Offloading.md) to accelerators.

>**Note**  
>Loop fission introduces overheads (e.g. loop control increment and branching),
>so in general it is necessary to run and benchmark the code to determine if
>loop fission brings performance gain.
