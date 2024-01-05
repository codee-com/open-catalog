# RMK016: Tune compiler optimization flags to avoid potential changes in floating-point precision

### Issue

Inappropriate usage of compiler optimization flags can have a direct impact on
the precision of floating point operation results.

### Actions

Consider using the `-O3` optimization flag instead of `-Ofast`, since the use of
`-Ofast` may have a negative impact on floating point operation precision.

### Relevance

Maximum optimization levels like `-Ofast` enable aggressive optimizations, which
have the potential to aggressively reduce the runtime but also can affect the
result of the executable code. The aforementioned flag enables optimizations of
floating-point instructions, which can introduce round-off errors that affect
floating-point precision, and it might even invalidate the results of the code.
Best practices for performance optimization typically recommend thoroughly
testing the correctness of the code when using these maximum optimization
levels.

In GCC compilers, most optimizations are completely disabled by default, which
is equivalent to setting the `-O0` flag. This prevents potential changes in
floating-precision, but it also disables performance optimizations carried out
by `-O1`, `-O2` and `-O3`.

### References

* [GCC compiler optimization options](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html)
