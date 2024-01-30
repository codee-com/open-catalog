# PWD002: Unprotected multithreading reduction operation

### Issue

A variable subject to a reduction operation is not properly handled in a
multithreading environment. This introduces a race condition making the result
of the code unpredictable.

### Actions

Protect the reduction operation.

### Relevance

A reduction operation is a typical computation pattern where multiple
computations are merged into a single value using a mathematical operation. When
those computations are executed in parallel, a race condition exists. To prevent
this, the variable where the reduction is performed must be protected.

### Code example

In the following code, there is a race condition for the `sum` variable since
its read/write operations are not properly protected.

```c
void foo() {
  int array[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int sum = 0;

  #pragma omp parallel for default(none) shared(array, sum)
  for (int i = 0; i < 10; i++) {
    sum += array[i];
  }
}
```

Shared data scoping should not be used in this case without protecting the
reduction operation. To do so, add an atomic directive, which ensures that only
one thread performs the read/write operation at the same time.

```c
void foo() {
  int array[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int sum = 0;

  #pragma omp parallel for default(none) shared(array, sum)
  for (int i = 0; i < 10; i++) {
    #pragma omp atomic update
    sum += array[i];
  }
}

```

Alternatively, the scalar reduction OpenMP clause can be used. This will
automatically privatize the specified variable so that each thread performs the
computation safely. Once all threads have finished, their results are reduced
into the variable using the specified reduction operator.

```c
void foo() {
  int array[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int sum = 0;

  #pragma omp parallel for default(none) shared(array) reduction(+: sum)
  for (int i = 0; i < 10; i++) {
    sum += array[i];
  }
}
```

### Related resources

* [PWD002 examples at GitHub](/Checks/PWD002)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked May 2019]

### References

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
