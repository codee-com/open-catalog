# PWR019: Consider interchanging loops to favor vectorization by maximizing inner loop's trip count

### Issue

Performance can be increased by using the highest possible trip count in the
[vectorized loop](/Glossary/Vectorization.md).

### Actions

Interchange loops so that the loop with the highest trip count becomes the
innermost loop.

### Relevance

Vectorization takes advantage of having as high a trip count (ie. number of
iterations) as possible. When loops are
[perfectly nested](/Glossary/Perfect-loop-nesting.md) and they can be safely
interchanged, making the loop with the highest trip count the innermost should
increase [vectorization](/Glossary/Vectorization.md) performance.

>**Notes**<br>
>If the loop interchange introduces non-sequential memory accesses, the runtime
>can get slower because of the inefficient memory access pattern.

### Code example

The following code shows two nested loops, where the outer one has a larger trip
count than the inner one:

```c
for (int i = 0; i < n; i++) {
  for (int j = margin; j < n - margin; j++) {
    bb[i][j] = 0.0;
    for (int k = -margin; k < margin; k++) {
      bb[i][j] += aa[i][j + k];
    }
  }
}
```

The value of `margin` is not known at compile time, but it is typically low. We
can increase the loop trip count of the innermost loop by performing loop
interchange. To do loop interchange, the loop over `j` and the loop over `k`
need to be perfectly nested. We can make them perfectly nested by moving the
initialization `bb[i][j] = 0.0` into a separate loop.

```c
for (int i = 0; i < n; i++) {
  for (int j = margin; j < n - margin; j++) {
    bb[i][j] = 0.0;
  }

  for (int k = -margin; k < margin; k++) {
    for (int j = margin; j < n - margin; j++) {
      bb[i][j] += aa[i][j + k];
    }
  }
}
```

### Related resources

* [PWR019 examples at GitHub](/Checks/PWR019)

### References

* [Vectorization](/Glossary/Vectorization.md)

* [Loop interchange](/Glossary/Loop-interchange.md)
