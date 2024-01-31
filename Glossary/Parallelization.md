# Parallelization

Parallelization is a process of **solving a computer problem by dividing it into
independent parts**, solving the parts independently, and finally, merging the
parts into a final solution.

Consider the following example:

```c
double sum = 0.0;
for (int i = 0; i < 1000; i++) {
  sum += a[i];
}
```

This code iterates from 0 to 999 and calculates the sum of elements of array
`a`. This problem can be solved in parallel. For example, we can split the
problem into four smaller subproblems, each smaller subproblem working on a part
of the array `a` which is 250 elements in size. The computer can independently
calculate the sum for each part of the array `a`, and when this is done, combine
the independently calculated sums into the final `sum`.

Not all problems can be parallelized easily, and the biggest obstacle to
parallelization are loop-carried dependencies.

There are several different types of hardware that can be used to speed up
computation through parallelization. The most famous are:

* [Vectorization](Vectorization.md): special unit of the CPU core that
can process more than one piece of data at a time.

* [Multithreading](Multithreading.md): distributing the parts of the
problem to several CPU cores for individual processing.

* [Offloading](Offloading.md): utilizing special massively-parallel
hardware architectures to solve the problems. These architectures are
specialized hardware, the most famous being GPUs.
