# PWR004: Declare OpenMP scoping for all variables

### Issue

Explicitly declare the scope of each variable used in a
[OpenMP](../../Glossary/OpenMP.md) `parallel` region to prevent an invalid
[default scoping](../../Glossary/Variable-scoping-in-the-context-of-OpenMP.md) being
assigned to it.

### Actions

Declare the scope of each variable through the appropriate OpenMP clause.

### Relevance

When the scope for a variable is not specified in an OpenMP `parallel` region, a
default scope is assigned to it. This default scope can be wrong, for instance
sharing a variable that should be private, which can lead to a race condition.
Furthermore, it ensures that the scope of each variable has been determined and
improves code readability.

### Code example

#### C

In the following code, a variable `factor` is used in each iteration of the
loop to initialize the array `result`:

```c
void example() {
  int factor = 42;
  int result[10];

  #pragma omp parallel for
  for (int i = 0; i < 10; i++) {
    result[i] = factor * i;
  }
}
```

Having the scope declared explicitly for each variable improves readability,
since it makes explicit the scope of all the variables within the parallel
region:

```c
void example() {
  int factor = 42;
  int result[10];

  #pragma omp parallel for default(none) shared(result, factor) private(i)
  for (int i = 0; i < 10; i++) {
    result[i] = factor * i;
  }
}
```

#### Fortran

In the following code, a variable `factor` is used in each iteration of the
loop to initialize the array `result`:

```fortran
subroutine example()
  integer :: factor = 42
  integer :: result(10)

  !$omp parallel do
  do i = 1, 10
    result(i) = factor * i
  end do
end subroutine example
```

Having the scope declared explicitly for each variable improves readability,
since it makes explicit the scope of all the variables within the parallel
region:

```fortran
subroutine example()
  integer :: factor = 42
  integer :: result(10)

  !$omp parallel do default(none) shared(factor, result) private(i)
  do i = 1, 10
    result(i) = factor * i
  end do
end subroutine example
```

### Related resources

* [PWR004 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR004/)

* [PWR005: Disable default OpenMP scoping](../PWR005/README.md)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked May 2019]

### References

* [Privatization](https://en.wikipedia.org/wiki/Privatization_(computer_programming))

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
