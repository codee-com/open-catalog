# PWR005: Disable default OpenMP scoping

### Issue

Disable default
[OpenMP scoping](../../Glossary/Variable-scoping-in-the-context-of-OpenMP.md) to
force having to declare the scoping of all variables.

### Actions

Add `default(none)` to disable default OpenMP scoping.

### Relevance

When the scope for a variable is not specified in an
[OpenMP](../../Glossary/OpenMP.md) `parallel` directive, a default scope is assigned
to it. Even when set explicitly, using a default scope is considered a bad
practice since it can lead to  wrong data scopes inadvertently being applied to
variables. Thus, it is recommended to explicitly set the scope for each
variable.

### Code example

In the following code, a variable `t` is used in each iteration of the loop to
hold a value that is then assigned to the array `result`. Since no data scoping
is declared for those variables the default will be used. This makes the
variable `t` shared which is incorrect since it introduces a race condition.

```c
void example() {
  int t;
  int result[10];

  #pragma omp parallel for
  for (int i = 0; i < 10; i++) {
    t = i + 1;
    result[i] = t;
  }
}
```

The following code disables the default scoping which will make the compiler
raise an error due to unspecified scopes.

```c
void example() {
  int t;
  int result[10];

  #pragma omp parallel for default(none)
  for (int i = 0; i < 10; i++) {
    t = i + 1;
    result[i] = t;
  }
}
```

To fix the code the scope of each variable must be specified. The variable `t`
must be made private to prevent the race condition.

```c
void example() {
  int t;
  int result[10];

  #pragma omp parallel for default(none) shared(result) private(t)
  for (int i = 0; i < 10; i++) {
    t = i + 1;
    result[i] = t;
  }
}
```

### Related resources

* [PWR005 examples](../PWR005)

* [PWR004: Declare variable OpenMP scoping](../PWR004/README.md)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked May 2019]

* [clang-tidy - openmp-use-default-none](https://clang.llvm.org/extra/clang-tidy/checks/openmp-use-default-none.html)
[last checked July 2019]

### References

* [Privatization](https://en.wikipedia.org/wiki/Privatization_(computer_programming))

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
