# PWR007: Disable implicit declaration of variables

### Issue

Implicit data typing is enabled by default and should be avoided since it is
error prone and reduces the legibility of the code.

### Actions

Add `implicit none` after the `program` or `module` declaration statement.

### Relevance

Since Fortran 77, variables can be implicitly declared by following certain
conventions. When no type is explicitly declared for a variable, the first
letter of its name is used to determine its type. Thus, when the first letter is
I, J, K, L, M, or N, then the data type is `integer`; otherwise, it is `real`.
This implicit behavior is discouraged since it is error prone and reduces the
legibility of the code. It can be disabled by adding an `implicit none`
statement.

> [!NOTE]
> Implicit data typing may lead to unexpected runtime errors. For example, it
> may change the results of the program due to implicit data typing determining
> the type of operation used in computations. Note the example code below shows
> issues with integer/real division operations due to implicit data typing.

> [!TIP]
> Starting with Fortran 2018, it is possible to use the extended form
> `implicit none (type, external)` to disable both implicit variables and
> implicit procedures, which are called through implicit interfaces. This
> approach mitigates the risks associated with such procedures, as outlined in
> [PWR068](/Checks/PWR068/).

### Code example

In the following example, the data type of all the variables is determined
implicitly:

```fortran
program example
  num1 = 7
  num2 = 2.5
  res = num1 / num2 ! res = 3.0
end program example
```

By disabling implicit data typing with the `implicit none` statement, the
compiler raises an error if the data types of all the variables is not declared
explicitly as follows:

```fortran
program example
  implicit none
  integer :: num1 = 7
  real :: num2 = 2.5, res
  res = num1 / num2 ! res = 2.799...
end program example
```

Note that the example code above probably fixes a precision issue as well. This
is due to implicit data typing of `num2`, which starts with the letter N and
thus it is implicitly assigned the `integer` type. This leads to the division
`num1 / num2` being an integer division operation (the `real` result `res`
equals `3.0`), instead of the probably intended real division operation (the
`real` result `res` equals `2.799….`).

### Related resources

* [PWR007 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR007/)

### References

* [IMPLICIT - FORTRAN 77 Language Reference, ORACLE](https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn9v/index.html),
2010 [last checked May 2023]

* [IMPLICIT - Intel® Fortran Compiler Developer Guide and Reference](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2023-2/overview.html),
April 2022 [last checked July 2023]
