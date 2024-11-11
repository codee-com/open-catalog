# PWR072: Split the variable initialization from the declaration to prevent the implicit 'save' behavior

### Issue

In Fortran, when a variable is initialized at its declaration, it implicitly
acquires the `save` attribute. This behavior is often unintended by the
programmer and can break the program logic.

### Actions

Split the initialization of the variable from its declaration to remove the
implicit `save` behavior and enhance code clarity.

> [!NOTE]
> If the `save` behavior is intentional, explicitly add the attribute in the
> variable declaration to clarify the intent:
>
> ```fortran
> integer, save :: count = 0
> ```

### Relevance

A variable with the `save` attribute retains its value across multiple
invocations of the procedure in which it is defined. The implicit `save`
behavior can cause debugging difficulties and lead to errors in program logic,
particularly when working in complex codebases where functions are called
multiple times during execution.

### Code example

Consider the code below, which computes the sum of the elements of various
arrays using the `sum_array()` function. Note how the variable `result` is set
to `0` at its declaration, implicitly acquiring the `save` behavior:

```fortran
! example.f90
program test_implicit_save
  implicit none
  integer, dimension(3) :: A = [1, 1, 1], B = [2, 2, 2]
  integer :: result

  result = sum_array(A)
  print *, "Sum of A:", result ! Expected: 3

  result = sum_array(B)
  print *, "Sum of B:", result ! Expected: 6

contains

  integer function sum_array(array)
    implicit none
    integer, intent(in) :: array(:)
    integer :: result = 0
    integer :: i

    do i = 1, size(array)
      result = result + array(i)
    end do

    sum_array = result
  end function sum_array

end program test_implicit_save
```

Each time `sum_array()` is called, one might expect `result` to be set to `0`
and then add the elements of the target array. However, `result` retains its
value between calls, breaking the intended logic for the program:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example.f90
$ ./a.out
 Sum of A:           3
 Sum of B:           9
```

While resolving the issue is as simple as splitting the initialization of
`result` to a separate line, this type of bug can be particularly challenging
to diagnose in complex codebases:

```fortran
! solution.f90
program test_implicit_save
  implicit none
  integer, dimension(3) :: A = [1, 1, 1], B = [2, 2, 2]
  integer :: result

  result = sum_array(A)
  print *, "Sum of A:", result ! Expected: 3

  result = sum_array(B)
  print *, "Sum of B:", result ! Expected: 6

contains

  integer function sum_array(array)
    implicit none
    integer, intent(in) :: array(:)
    integer :: result
    integer :: i

    result = 0

    do i = 1, size(array)
      result = result + array(i)
    end do

    sum_array = result
  end function sum_array

end program test_implicit_save
```

### Related resources

* [PWR072 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR072/)

### References

* ["Variables -- Fortran Programming
Language"](https://fortran-lang.org/en/learn/quickstart/variables/), Fortran
Community. [last checked April 2024]
