# PWR017: Using countable while loops instead of for loops may inhibit vectorization

### Issue

Using countable while loops instead of for loops may inhibit
[vectorization](../../Glossary/Vectorization.md).

### Actions

Rewrite the loop so that its number of iterations can be counted.

### Relevance

Many loops can benefit from automatic compiler
[vectorization](../../Glossary/Vectorization.md). However, `while` loops are more
difficult to vectorize than `for` loops. For a compiler to be able to vectorize
a loop, certain requirements need to be met. One of them being that the loop is
countable, i.e. the number of iterations is known before entering the loop so
that the compiler can chunk the data into vectors. Normally, compilers can
successfully count the number of iterations of most `for` loops; however, they
tend to struggle for other types such as `while` loops. This is especially true
for older compilers. Thus, the use of `for` loops is encouraged.

### Code example

Consider the following loop:

```c
int example(int *A) {
  int sum = 0;
  int i = 0;
  while (i < 1000) {
    sum += A[i++];
  }
  return sum;
}
```

The number of iterations for this loop is known before the loop starts
executing, and this loop is therefore countable. It can be converted to a `for`
loop, like this:

```c
int example(int *A) {
  int sum = 0;
  for (int i = 0; i < 1000; i++) {
    sum += A[i];
  }
  return sum;
}
```

### Related resources

* [PWR017 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR017/)

### References

* [Vectorization](../../Glossary/Vectorization.md)

* [OpenMP canonical form](../../Glossary/OpenMP-canonical-form.md)
