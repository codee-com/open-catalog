# PWR082: Remove unused variables

### Issue

A variable that is declared but never used adds noise: it increases the amount
of information to keep track of, suggesting data flow that does not actually
exist. In some cases, an unused variable may instead hint at a hidden defect in
the program's logic, such as a missing assignment or an incomplete computation.

### Actions

Determine whether each unused variable is accidentally or intentionally unused;
remove those that are not needed. If a variable is intentionally unused but,
for any reason, it cannot be removed, make that intent explicit using
language-specific mechanisms.

### Relevance

Fortran, C, and C++ allow variables to be declared without ever being used.
Eliminating unused variables reduces cognitive load and avoids suggesting
nonexistent data flow, helping readers focus on what actually contributes to
the program's behavior.

If a variable `x` must remain intentionally unused (e.g., an argument required
in an API call but unused in a particular implementation), it is possible to
document that intent using language constructs, such as:

- **C:** `[[maybe_unused]] int x` (since C23), or compiler-specific annotations
  such as `gcc`'s `__attribute__((unused))`. Another common alternative,
  although less explicit, is to "consume" the variable with a cast `(void) x`.

- **C++:** `[[maybe_unused]] int x` (since C++17).

- **Fortran:** There is no standard attribute, and compilers such as `gfortran`
  do not provide attributes for annotation. A simple and portable approach is
  to create a reference with no runtime effect:

```fortran
associate(unused => x)
end associate
```

### Code examples

#### C

In the following code, the variable `t` is declared but never used to perform
the computation:

```c {3} showLineNumbers
// example.c
__attribute__((pure)) int sumArray(const int n, const int *array) {
  int t, sum = 0;

  for (int i = 0; i < n; ++i) {
    sum += array[i];
  }

  return sum;
}
```

Thus, `t` should be removed:

```c {3} showLineNumbers
// solution.c
__attribute__((pure)) int sumArray(const int n, const int *array) {
  int sum = 0;

  for (int i = 0; i < n; ++i) {
    sum += array[i];
  }

  return sum;
}
```

#### Fortran

In the following code, the variable `t` is declared but never used to perform
the computation:

```fortran {5} showLineNumbers
! example.f90
pure function sumArray(array) result(sum)
  implicit none
  integer, intent(in) :: array(:)
  integer :: t, sum, i

  sum = 0

  do i = 1, size(array)
    sum = sum + array(i)
  end do
end function sumArray
```

Thus, `t` should be removed:

```fortran {5} showLineNumbers
! solution.f90
pure function sumArray(array) result(sum)
  implicit none
  integer, intent(in) :: array(:)
  integer :: sum, i

  sum = 0

  do i = 1, size(array)
    sum = sum + array(i)
  end do
end function sumArray
```

### Related resources

- [PWR082
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR082/)

### References

- ["Common Variable Attributes (Using the GNU Compiler Collection
  (GCC))"](https://gcc.gnu.org/onlinedocs/gcc/Common-Variable-Attributes.html#index-unused-variable-attribute),
  Free Software Foundation, Inc. [last checked February 2026]

- ["C attribute: maybe_unused (since
  C23)"](https://en.cppreference.com/w/c/language/attributes/maybe_unused.html),
  cppreference.com. [last checked February 2026]

- ["C++ attribute: maybe_unused (since
  C++17)"](https://en.cppreference.com/w/cpp/language/attributes/maybe_unused.html),
  cppreference.com. [last checked February 2026]
