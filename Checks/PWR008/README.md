# PWR008: Declare the intent for each procedure parameter

### Issue

Each procedure parameter should have its intent declared to facilitate reasoning
about the flow of data in and out of the function.

### Actions

Declare the proper intent for each procedure parameter.

### Relevance

By declaring the intent of each procedure parameter the flow of data in and out
of the function is made explicit in the source code. This not only improves code
legibility but also allows reasoning about what the procedure does: by including
all data inputted and outputted by a procedure in its signature, the
interactions of the procedure with external data can be easily determined by
inspecting the procedure call sites. Compilers and other code analysis tools
take advantage of this information to enforce correctness, performance and
programming best practices.

>**Note**  
>Implicit intent of variables may lead to unexpected runtime errors. For
>example, it may avoid the detection of errors in the code by the compiler. Note
>the example code below shows issues with incorrect assignment of variables and
>incorrect mapping of variables in the call site of the function.

### Code example

In the following example, the intent of all the parameters of the function is
not explicit in the code:

```f90
PROGRAM example
  IMPLICIT NONE
  INTEGER :: s = 2
  CALL foo(s, 2)
CONTAINS
  SUBROUTINE foo(a, b)
    IMPLICIT NONE
    INTEGER :: a
    INTEGER :: b
    a = 5
    b = a * 2
  END SUBROUTINE foo
END PROGRAM example
```

By enforcing the explicit declaration of the intent of the parameters of the
function, the source code looks as follows:

```f90
PROGRAM example
  IMPLICIT NONE
  INTEGER :: s = 2
  CALL foo(s, 2)
CONTAINS
  SUBROUTINE foo(a, b)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: a
    INTEGER, INTENT(OUT) :: b
    a = 5
    b = a * 2
  END SUBROUTINE foo
END PROGRAM example
```

Note that the example code above raises two runtime errors due to the incorrect
usage of variable's intent. For example, a variable like `a` with an
`INTENT(IN)` cannot be assigned inside the function. Another example is variable
`b` with an `INTENT(OUT)` which cannot be mapped to a constant `2` at the
function call site `CALL foo(s, 2)`.

### Related resources

* [PWR008 examples at GitHub](/Checks/PWR008)

### References

* [INTENT - Intel® Fortran Compiler Developer Guide and Reference](https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference/h-to-i/intent.html),
April 2022 [last checked May 2023]
