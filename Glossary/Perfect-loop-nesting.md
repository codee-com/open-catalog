# Perfect loop nesting

In the context of program optimizations, **perfect loop nesting** happens when
all the statements in a loop nest of two or more loops are in the innermost
loop.

Consider following example:

```c showLineNumbers
for (int i = 0; i < n; i++) {
  for (int j = 0; i < n; i++) {
    c[i] += a[j] + b[j][i];
  }
}
```

In the above example of two nested loops, all the statements are in the
innermost loop. We say that the loops are perfectly nested. Contrast this with
the following example:

```c {2} showLineNumbers
for (int i = 0; i < n; i++) {
  c[i] = 0.0;
  for (int j = 0; i < n; i++) {
    c[i] += a[j] + b[j][i];
  }
}
```

In this example, the loops are not perfectly nested because of the statement
`c[i] = 0.0` on line 2.

Perfect-loop nesting is very important for loop optimizations, since it enables
other optimization techniques like
[loop interchange](Loop-interchange.md) or
[loop tiling](Loop-tiling.md).

### Converting imperfectly-nested loops to perfectly nested

As already mentioned, perfect loop nesting is a prerequisite for loop
interchange. Luckily, many times imperfectly nested loops can be converted to
perfectly nested. Here are three examples:

#### Example 1: Isolating statements that prevent perfect loop nesting into a separate loop

```c {2} showLineNumbers
for (int i = 0; i < n; i++) {
  c[i] = 0.0;
  for (int j = 0; i < n; i++) {
    c[i] += a[j] + b[j][i];
  }
}
```

This loop can be made perfectly nested by extracting the statement `c[i] = 0.0`
on line 2 into a separate loop, like this:

```c {5-9} showLineNumbers
for (int i = 0; i < n; i++) {
  c[i] = 0.0;
}

for (int i = 0; i < n; i++) {
  for (int j = 0; i < n; i++) {
    c[i] += a[j] + b[j][i];
  }
}
```

The loop nest on lines 5-9 is perfectly nested and can profit from other
optimizations.

#### Example 2: Promoting temporary scalars to vectors to enable perfect loop nesting

A second example of imperfectly nested loops:

```c {2,3,8} showLineNumbers
for (int i = 0; i < n; i++) {
  double sum = 0.0;
  double dot = 0.0;
  for (int j = 0; i < n; i++) {
    sum += a[j] + b[j][i];
    dot += a[j] * b[i][j];
  }
  c[i] = dot / sum;
}
```

In this example, three statements on lines 2, 3 and 8 prevent perfect loop
nesting. Conversion to perfect loop nesting is possible by promoting scalar
variables `sum` and `dot` to arrays and splitting the loop into three smaller
loops:

```c {9-14} showLineNumbers
double *sum_arr = malloc(sizeof(double) * n);
double *dot_arr = malloc(sizeof(double) * n);

for (int i = 0; i < n; i++) {
  sum_arr[i] = 0.0;
  dot_arr[i] = 0.0;
}

for (int i = 0; i < n; i++) {
  for (int j = 0; i < n; i++) {
    sum_arr[i] += a[j] + b[j][i];
    dot_arr[i] += a[j] * b[i][j];
  }
}

for (int i = 0; i < n; i++) {
  c[i] = dot_arr[i] / sum_arr[i];
}

free(sum_arr);
free(dot_arr);
```

The loop nest on lines 9-14 is now perfectly nested and can be optimized
further.

#### Example 3

The last example looks like this:

```c {2,3} showLineNumbers
for (int i = 0; i < n; i++) {
  double *a_ptr = a + n * i;
  double *b_ptr = b + i;

  for (int j = 0; j < n; j++) {
    *a_ptr = *b_ptr;
    a_ptr++;
    b_ptr += n;
  }
}
```

This example uses pointer-based notation to access the elements of the array. By
converting the index-based notation, we can eliminate the statements on lines 2
and 3 preventing perfect loop nesting:

```c showLineNumbers
for (int i = 0; i < n; i++) {
  for (int j = 0; j < n; j++) {
    a[j + i * n] = b[i + j * n];
  }
}
```

By moving to the index based notations, we made the loops perfectly nested with
the potential to enable other optimizations.

### Related resources

* [Loop interchange](Loop-interchange.md)

* [Loop tiling](Loop-tiling.md)
