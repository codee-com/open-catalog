# Locality of reference

In modern computer systems, the memory subsystem is often the bottleneck.
Understanding the way the memory subsystem works and adjusting a piece of code
to accommodate it can yield significant performance improvements.

The memory subsystem consists of several levels of data cache, intended to speed
up access to the commonly used data. The data cache is smaller than the main
memory and therefore can only hold a part of the data. Access to the data cache
is much faster than access to the main memory.

The data caches are built on assumptions that programs behave in the following
ways:

* **Temporal locality**: If the CPU has accessed a certain memory location, there
is a high probability that it will access it again in the near future. Using the
same values in different loop iterations is an example of temporal locality.

* **Spatial locality**: If the CPU has accessed a certain memory location, there
is a high probability that it will access its neighboring locations in the near
future. Iterating through an array is an example of spatial locality.

The common name for both assumptions is **locality of reference**.

The data that hasn't been accessed for some time by the CPU is evicted from the
cache to make room for the new data. It is therefore important for a piece of
code to reuse the data while in the data cache and before it gets evicted if
aiming for optimum performance.

The developers can use techniques such as
[loop interchange](Loop-interchange.md) or
[loop tiling](Loop-tiling.md) to increase the loop's locality of
reference, which as an effect yields an increase in speed.

### Better memory efficiency through loop fission

Writing code that makes efficient use of memory is essential to write performant
code for modern hardware. For example, loop fission enables writing smaller
loops to achieve a better utilization of locality of reference. It can be more
efficient in multithreaded code, as individual threads may benefit from single-
core optimizations when running on multi-core processors. Note loop fission
introduces overheads (e.g. loop control increment and branching), so in general
it is necessary to run and benchmark the code to determine if loop fission
brings performance gain.

### Enabling vectorization through loop fission

Writing code that makes efficient use of vectorization is essential to write
performant code for modern hardware. For example, loop fission enables splitting
a non-vectorizable loop into two or more loops. The goal of the fission is to
isolate the statements preventing the vectorization into a dedicated loop. By
doing this, we enable vectorization in the rest of the loop, which can lead to
speed improvements. Note loop fission introduces overheads (e.g. loop control
increment and branching), so in general it is necessary to run and benchmark the
code to determine if loop fission brings performance gain.

### Loop fission and offloading to accelerators

Typically, the term offloading refers to moving the execution of a program or a
part of the program from the CPU to hardware accelerators, such as GPUs. In
contrast to CPUs, which offer limited parallelism, accelerators and GPUs are
massively-parallel architectures that can speed up solving certain types of
problems. The CPU and accelerators have distinct memories, and the major
limiting factor to offloading performance is data movements, that is moving data
from the memory of one to the memory of the other.

Writing performant code for accelerators is a complex time-consuming
undertaking. Efficient GPU code must make an efficient use of memory through
good locality of reference, of vectorization to enable fast execution in the GPU
cores, and of parallelism to exploit the computational power of the thousands of
GPU cores typically available in the hardware. Thus, loop fission offers a
useful code transformation in the scope of GPU programming.
