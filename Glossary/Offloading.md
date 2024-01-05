# Offloading

In the context of parallel programming, the term **offloading** refers to moving
the execution of a program or a part of the program from the CPU to hardware
accelerators, such as GPUs.

In contrast to CPUs, which offer limited parallelism, accelerators and GPUs are
massively-parallel architectures that can speed up solving certain types of
problems.

The CPU and accelerators have distinct memories, and the major limiting factor
to offloading performance is data movements, that is moving data from the memory
of one to the memory of the other.
