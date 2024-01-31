# Multithreading

Modern CPUs very often have more than one processing cores, which can be used
for parallelization. In this context, multithreading means splitting the
workload into several parts. Each part is then processed independently by a CPU
core. The term **multithreading** is used to designate splitting the workload
to several CPU cores in order to speed up its execution.

The crucial underlying concept of multithreading is **thread**. The simplest way
to imagine a thread is as an independent worker, which has its own code that it
is executing. Some of the data used by the thread is local to the thread, and
some of it is shared among all threads. An important aspect of multithreading is
that all the threads in principle have access to the same address space.

Although the user can create as many logical threads as they want, for optimum
performance the number of threads should correspond to the number of CPU cores.

There are several interfaces the threads are exposed to the developers:

* **Raw threads** (e.g. pthreads in C or `std::thread` in C++): this interface
is the most basic, because the developer is explicitly in charge of splitting
the problem into pieces, distributing it to threads and thread synchronization,
but it is the most powerful.

* **OpenMP Multithreading**: OpenMP multithreading is a set of compiler pragmas
supported by most compilers. By applying these pragmas to the source code, the
developer provides the information to the compiler which feeds the OpenMP
runtime. OpenMP runtime is in charge of splitting the workload into smaller
segments, distributing it to CPU cores and thread synchronization.

* **Parallel algorithms and frameworks:** There are many frameworks that offer
parallelization out-of-the-box, without the need for the user to do anything.
For example, starting from C++17, many algorithms come with parallel versions,
e.g. `std::sort` for sorting, `std::transform` for applying transformation
function to each element of the array, etc.

The two biggest challenges with multithreading are:

1. [Deciding which data should be thread-private and which should be shared](Variable-scoping-in-the-context-of-OpenMP.md),

2. and thread synchronization and possible data races. Without it the parallelization either doesn't pay off in term of performance or gives the wrong results.
