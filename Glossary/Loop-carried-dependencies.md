# Loop-carried dependencies

For a piece of code to be successfully parallelized, one needs to be able to
split it into independent parts. Not all codes can be easily parallelized, and
the biggest obstacle to parallelization are **loop-carried dependencies**.

To illustrate what loop-carried dependencies are, consider the following loop:

```c
for (int i = 2; i < 1000; i++) {
  a[i] = a[i-1] + a[i - 2];
}
```

The loop calculates the value of `a[i]` by using the values computed in the
previous two iterations of the loop, `i - 1` and `i - 2`. The values needed to
calculate `a[i - 1]` are calculated in iterations `i - 2` and `i - 3`, etc. This
kind of problem cannot be easily split into smaller subproblems and then
subproblems solved independently: there is a chain of dependencies going back to
the beginning of the loop which effectively prevents parallelization.

This type of dependencies are called **loop-carried dependencies**, and it is
the main limiting factor in the possibility of parallelization.
