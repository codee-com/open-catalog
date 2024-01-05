# OpenMP

**OpenMP** is an application programming interface (API) for shared-memory
multiprocessor system for C, C++ and Fortran. It is supported by all major
compilers on many different platforms and operating systems.

The idea behind OpenMP is simple: the developer writes the code in the way they
are used to, but in addition they use special OpenMP compiler pragmas to provide
the information needed to run the code on a multithreaded system.

The OpenMP runtime is in charge of distributing the problem onto several CPU
cores, data movements and data synchronization. The model is lightweight and has
found usage in many domains where multiprocessor programming is important.

Originally, OpenMP was created as an API for shared-memory multiprocessor
systems. From OpenMP 4.0, it also supports [offloading](/Glossary/Offloading.md)
the computations to accelerators.
