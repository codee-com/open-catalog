# PWR049: Move iterator-dependent condition outside of the loop

### Issue

A condition that depends only on the iterator variable can be moved outside of
the loop.

### Actions

Move iterator-dependent condition outside of the loop.

### Relevance

A condition that depends only on the iterator is predictable: we know exactly at
which iteration of the loop it is going to be true. Nevertheless, it is
evaluated in each iteration of the loop.

Moving the iterator-dependent condition outside of the loop will result in fewer
instructions executed in the loop. This transformation can occasionally enable
vectorization, and for the loops that are already vectorized, it can increase
vectorization efficiency.

>**Notes**  
>Moving an iterator-dependent condition outside of the loop is a creative
>process. Depending on the type of condition, it can involve loop peeling,
>[loop fission](/Glossary/Loop-fission.md) or loop unrolling.

### Code examples

#### Example 1

Have a look at the following simple code:

```c
for (int i = 0; i < n; ++i) {
  if (i = 0) {
    a[i] = 0;
  } else {
    a[i] = 1;
  }
}
```

The condition on line 2 depends on the iterator `i` and can be removed by
computing the first array element `a[0]` outside the loop. Thus, the loop
iterator starts in 1 and the loop initializes the remaining array elements
without computing any conditional statement:

```c
a[0] = 0;
for (int i = 1; i < n; ++i) {
  a[i] = 1;
}
```

The iterator-dependent condition can appear in more complicated loops as well.
For illustrative purposes, an example code with a loop nest is shown below:

```c
for (int i = 0; i < n; ++i) {
  for (int j = 0; j < n; ++j) {
    if (i == 0) {
      a[i][j] = 0;
    } else {
      a[i][j] = a[i][j - 1] + b[i][j];
    }
  }
}
```

The condition on line 3 depends on the iterator `i` of the outer loop and can be
removed from the inner loop as follows:

```c
for (int i = 0; i < n; ++i) {
  a[i][0] = 0;
  for (int j = 1; j < n; ++j) {
    a[i][j] = a[i][j - 1] + b[i][j];
  }
}
```

In the example codes shown above the resulting loops are branchless, avoiding
redundant computations of predictable conditional instructions.

#### Example 2: Loop fission

Have a look at the following code:

```c
for (int i = 0; i < n; ++i) {
  if (i < 10) {
    a[i] = 0;
  } else {
    a[i] = 1;
  }
}
```

The condition on line 2 depends on the iterator `i` and can be removed by
splitting the inner loop over `i` into two loops:

```c
for (int i = 0; i < 10; ++i) {
  a[i] = 0;
}
for (int i = 10; i < n; ++i) {
  a[i] = 1;
}
```

The first loop iterates from `0` to `9`, and the second loop iterates from `10`
until `n`. The condition is removed from the loop.

#### Example 3: Loop unrolling

Here is another example of a iterator-dependent condition in the loop body:

```c
for (int i = 0; i < n; ++i) {
  if (i % 2 == 0) {
    a[i] = 1;
  } else {
    a[i] = 0;
  }
}
```

The iterator-dependent condition is on line 2, and can be removed through loop
unrolling:

```c
for (int i = 0; i < n; i += 2) {
  a[i] = 1;
  a[i + 1] = 0;
}
```

Loop unrolling changes the increment of iterator variable `i`, so now it is 2
(see loop header at line 1). The condition is gone after this modification.

### Related resources

* [PWR049 examples at GitHub](/Checks/PWR049)

### References

* [Loop unswitching](/Glossary/Loop-unswitching.md)

* [Loop fission](/Glossary/Loop-fission.md)
