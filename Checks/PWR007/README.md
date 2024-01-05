# PWR007: Disable implicit declaration of variables

### Issue

Implicit data typing is enabled by default and should be avoided since it is
error prone and reduces the legibility of the code.

### Actions

Add `IMPLICIT NONE` after the `PROGRAM` or `MODULE` declaration statement.

### Relevance

Since Fortran 77, variables can be implicitly declared by following certain
conventions. When no type is explicitly declared for a variable, the first
letter of its name is used to determine its type. Thus, when the first letter is
I, J, K, L, M, or N, then the data type is `INTEGER`; otherwise, it is `REAL`.
This implicit behavior is discouraged since it is error prone and reduces the
legibility of the code. It can be disabled by adding an `IMPLICIT NONE`
statement.

>**Note**  
>Implicit data typing may lead to unexpected runtime errors. For example, it may
>change the results of the program due to implicit data typing determining the
>type of operation used in computations. Note the example code below shows
>issues with integer/real division operations due to implicit data typing.

### Code example

In the following example, the data type of all the variables is determined
implicitly:

```f90
PROGRAM example
  NUM1 = 7
  NUM2 = 2.5
  RES = NUM1 / NUM2 ! RES = 3.0
END PROGRAM example
```

By disabling implicit data typing with the `IMPLICIT NONE` statement, the
compiler raises an error if the data types of all the variables is not declared
explicitly as follows:

```f90
PROGRAM example
  IMPLICIT NONE
  INTEGER :: NUM1 = 7
  REAL :: NUM2 = 2.5, RES
  RES = NUM1 / NUM2 ! RES = 2.799...
END PROGRAM example
```

Note that the example code above probably fixes a precision issue as well. This
is due to implicit data typing of `NUM2`, which starts with the letter N and
thus it is implicitly assigned the `INTEGER` type. This leads to the division
`NUM1 / NUM2` being an integer division operation (the `REAL` result `RES`
equals `3.0`), instead of the probably intended real division operation (the
`REAL` result `RES` equals `2.799….`).

### Related resources

* [PWR007 examples at GitHub](/Checks/PWR007)

### References

* [IMPLICIT - FORTRAN 77 Language Reference, ORACLE](https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn9v/index.html),
2010 [last checked May 2023]

* [IMPLICIT - Intel® Fortran Compiler Developer Guide and Reference](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2023-2/overview.html),
April 2022 [last checked July 2023]
