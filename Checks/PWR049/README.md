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
>[loop fission](../../Glossary/Loop-fission.md) or loop unrolling.

### Code examples

#### C

##### Example 1

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
    if (j == 0) {
      a[i][j] = 0;
    } else {
      a[i][j] = a[i][j - 1] + b[i][j];
    }
  }
}
```

The condition on line 3 depends on the iterator `j` of the inner loop and can
be removed as follows:

```c
for (int i = 0; i < n; ++i) {
  a[i][0] = 0;

  for (int j = 1; j < n; ++j) {
    a[i][j] = a[i][j - 1] + b[i][j];
  }
}
```

In the example codes shown above, the resulting loops are branchless, avoiding
redundant computations of predictable conditional instructions.

##### Example 2: Loop fission

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
splitting the loop over `i` into two loops:

```c
for (int i = 0; i < 10; ++i) {
  a[i] = 0;
}

for (int i = 10; i < n; ++i) {
  a[i] = 1;
}
```

The first loop iterates from `0` to `9`, and the second loop iterates from `10`
until `n - 1`. The condition is removed from the loop.

##### Example 3: Loop unrolling

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

#### Fortran

##### Example 1

```f90
do i = 1, size(a, 1)
  if (i == 1) then
    a(i) = 0
  else
    a(i) = 1
  end if
end do
```

The condition on line 2 depends on the iterator `i` and can be removed by
computing the first array element `a(1)` outside the loop. Thus, the loop
iterator starts in 2 and the loop initializes the remaining array elements
without computing any conditional statement:

```f90
a(1) = 0

do i = 2, size(a, 1)
  a(i) = 1
end do
```

The iterator-dependent condition can appear in more complicated loops as well.
For illustrative purposes, an example code with a loop nest is shown below:

```f90
do j = 1, size(a, 2)
  do i = 1, size(a, 1)
    if (i == 1) then
      a(i, j) = 0
    else
      a(i, j) = a(i - 1, j) + b(i, j)
    end if
  end do
end do
```

The condition on line 3 depends on the iterator `i` of the inner loop and can
be removed as follows:

```f90
do j = 1, size(a, 2)
  a(1, j) = 0

  do i = 2, size(a, 1)
    a(i, j) = a(i - 1, j) + b(i, j)
  end do
end do
```

In the example codes shown above, the resulting loops are branchless, avoiding
redundant computations of predictable conditional instructions.

##### Example 2: Loop fission

```f90
do i = 1, size(a, 1)
  if (i < 10) then
    a(i) = 0
  else
    a(i) = 1
end do
```

The condition on line 2 depends on the iterator `i` and can be removed by
splitting the loop over `i` into two loops:

```f90
do i = 1, 9
  a(i) = 0
end do

do i = 10, size(a, 1)
  a(i) = 1
end do
```

The first loop iterates from `1` to `9`, and the second loop iterates from `10`
until `size(a, 1)`. The condition is removed from the loop.

##### Example 3: Loop unrolling

Here is another example of a iterator-dependent condition in the loop body:

```f90
do i = 1, size(a, 1)
  if (modulo(i, 2) == 0) then
    a(i) = 1
  else
    a(i) = 0
  end if
end do
```

The iterator-dependent condition is on line 2, and can be removed through loop
unrolling:

```f90
do i = 1, size(a, 1), 2
    a(i) = 0
    a(i + 1) = 1
end do
```

Loop unrolling changes the increment of iterator variable `i`, so now it is 2
(see loop header at line 1). The condition is gone after this modification.

### Related resources

* [PWR049 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR049/)

### References

* [Loop unswitching](../../Glossary/Loop-unswitching.md)

* [Loop fission](../../Glossary/Loop-fission.md)
