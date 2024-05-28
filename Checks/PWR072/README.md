# PWR072: Add an explicit save attribute when initializing variables in their declaration

### Issue

In Fortran, when a variable is initialized at its declaration, it implicitly
acquires the `save` attribute. This behavior can lead to unintended
consequences if the programmer is unaware of it.

### Actions

To improve code clarity, explicitly write the `save` attribute when
initializing variables at their declaration.

### Relevance

A variable with the `save` attribute retains its value across multiple
invocations of the procedure in which it is defined. The implicit `save`
behavior can cause debugging difficulties and lead to errors in program logic,
particularly when working in complex codebases where functions are called
multiple times during execution.

### Code example

Consider the subroutine below, where a variable `count` is initialized at its
declaration, but without an explicit `save` attribute:

```f90
! example.f90
program test_implicit_save
  call counter
  call counter

contains

subroutine counter()
  integer :: count = 0
  count = count + 1
  print *, count
end subroutine
end program test_implicit_save
```

Each time `counter()` is called, one might expect `count` to be set to `0` and
then incremented to `1`. However, due to the implicit `save` attribute, `count`
retains its value between calls:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example.f90
$ ./a.out
           1
           2
```

A clearer and more intentional approach would be:

```f90
! solution.f90
program test_explicit_save
  call counter
  call counter

contains

subroutine counter()
  integer, save :: count = 0
  count = count + 1
  print *, count
end subroutine
end program test_explicit_save
```

Here, the `save` attribute is explicitly declared, making it clear that `count`
is intended to accumulate its value between calls.

>**Note:**  
>If you simply want to initialize a variable when declaring it, while avoiding
>the `save` behavior, put the initialization in a separate line:
>
>```f90
>integer :: count
>count = 0
>```

### Related resources

* [PWR072 source code examples](../PWR072)

### References

* ["Variables -- Fortran Programming
Language"](https://fortran-lang.org/en/learn/quickstart/variables/), Fortran
Community. [last checked April 2024]
