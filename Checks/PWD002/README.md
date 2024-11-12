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

#### C

In the following code, the variable `sum` is subjected to a shared data
scoping. This introduces a race condition on the variable, since its read/write
operations are not properly protected:

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

To protect the reduction operation, we can add an `atomic` directive from
OpenMP, which ensures that only one thread performs the read/write operation at
a given time:

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

Alternatively, we can also use the scalar `reduction` directive from OpenMP.
This will automatically create a copy of the variable for each thread, ensuring
they can perform their computations safely. Once all threads have finished,
their results are combined into the original variable using the specified
reduction operator:

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

#### Fortran

In the following code, the variable `sum` is subjected to a shared data
scoping. This introduces a race condition on the variable, since its read/write
operations are not properly protected:

```f90
subroutine example(array)
  integer, intent(in) :: array(:)
  integer :: i, sum

  sum = 0

  !$omp parallel do default(none) shared(array, sum)
  do i = 1, size(array, 1)
    sum = sum + array(i)
  end do
end subroutine example
```

To protect the reduction operation, we can add an `atomic` directive from
OpenMP, which ensures that only one thread performs the read/write operation at
a given time:

```f90
subroutine example(array)
  integer, intent(in) :: array(:)
  integer :: i, sum

  sum = 0

  !$omp parallel do default(none) shared(array, sum)
  do i = 1, size(array, 1)
    !$omp atomic update
    sum = sum + array(i)
  end do
end subroutine example
```

Alternatively, we can also use the scalar `reduction` directive from OpenMP.
This will automatically create a copy of the variable for each thread, ensuring
they can perform their computations safely. Once all threads have finished,
their results are reduced into the original variable using the specified
reduction operator:

```f90
subroutine example(array)
  integer, intent(in) :: array(:)
  integer :: i, sum

  sum = 0

  !$omp parallel do default(none) shared(array) reduction(+: sum)
  do i = 1, size(array, 1)
    sum = sum + array(i)
  end do
end subroutine example
```

### Related resources

* [PWD002 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWD002/)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked May 2019]

### References

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
