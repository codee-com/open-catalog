# Loop-carried dependencies

For a piece of code to be successfully parallelized, it needs to be split into
independent parts. Not all code can be easily parallelized, and the biggest
obstacle to parallelization is the presence of **loop-carried dependencies**.

To illustrate what loop-carried dependencies are, consider the following loop:

```c
for (int i = 2; i < 1000; i++) {
  a[i] = a[i - 1] + a[i - 2];
}
```

This loop calculates the value of `a[i]` using the values computed in the
previous two iterations (`i - 1` and `i - 2`). The values needed to calculate
`a[i - 1]` are themselves computed in iterations `i - 2` and `i - 3`, and so on.
This kind of problem cannot be easily split into smaller independent
subproblems because there is a chain of dependencies going back to the beginning
of the loop, which effectively prevents parallelization.
