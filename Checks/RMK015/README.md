# RMK015: Tune compiler optimization flags to increase the speed of the code

### Issue

The appropriate usage of the compiler optimization flags has a direct impact on
the speed of the executable code.

### Actions

Consider using the `-O2` optimization flag to increase performance by reducing
the runtime of the executable code. Also consider using `-O3` being aware that
it may have some drawbacks with some compilers, such as a possible hindering of
debugging tools.

### Relevance

Compilers are designed to **convert source code into efficient executable code
for the target hardware**, **reducing the cost of the compilation process** and
**facilitating the debugging  process** by the programmer. Compilers provide
optimization flags to improve performance, as well as optimization flags for
reducing the size of the executable code. Typical compiler optimization flags
for performance are `-O0`, `-O1`, `-O2`, `-O3` and `-Ofast`. On the other hand,
a typical flag for reducing the code size is `-Os`.

Best practices for performance optimization **typically recommend using the
`-O2` flag**, as it makes the compiler attempt to reduce the runtime through
well-known programming techniques like loop unrolling,
[loop interchange](/Glossary/Loop-interchange.md),
[loop vectorization](/Glossary/Vectorization.md),
[loop fission](/Glossary/Loop-fission.md), loop fusion, etc. It is important to
note that the list of techniques actually used depends on the compiler (e.g.
GCC, clang) and the compiler version (e.g. GCC 11, GCC 12).

>**Note**  
>In GCC compilers, most optimizations are completely disabled by default, which
>is equivalent to setting the `-O0` flag. Invoking GCC with the
>`-Q --help=optimizers` flags makes it list the exact set of optimizations that
>are enabled at the optimization level chosen by the user. Note that from GCC
>version 12, the optimization level `-O2` includes loop vectorization, which was
>originally only available with -O3 (in previous versions of the compiler).

### References

* [GCC compiler optimization options](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html)
