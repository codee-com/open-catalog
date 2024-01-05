# Vectorization

Modern CPU cores have special vector units that can process more than one piece
of data in a single instruction. For example, a modern X86-64 CPU with AVX
vector instructions has a set of different instructions that can work with 4
double values in one instruction: vector load instruction loads 4 double values
into a vector register, vector add instruction adds together 2 registers each
holding 4 double values, etc.

The process of using special vector instructions to speed up computation is
called *vectorization*. If the code meets certain conditions, the compiler can
emit vector instructions instead of scalar instructions in order to speed up
computation. This process is called *autovectorization*.

For the compiler to be able automatically vectorize a loop, certain conditions
must be fulfilled:

Sometimes compilers fail to vectorize certain loops automatically for various
reasons. It is possible to use compiler pragmas either to force vectorization or
provide additional information needed for vectorization. There are portable
vectorization pragmas defined by
[OpenMP standard](https://www.openmp.org/spec-html/5.0/openmpsu42.html), but
also there are compiler-specific vectorization pragmas as well.

Apart from relying on autovectorization, it is possible for the developer to
manually write code that employs vector instructions: either pure assembly,
[C/C++ assembly intrinsics](https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html),
or using one of the vectorization frameworks such as
[EVE](https://github.com/jfalcou/eve).
