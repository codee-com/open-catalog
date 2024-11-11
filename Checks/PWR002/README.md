# PWR002: Declare scalar variables in the smallest possible scope

### Issue

A scalar variable should be declared in the smallest
[scope](../../Glossary/Variable-scope.md) possible. In computer programming, the term
scope of a variable usually refers to the part of the code where the variable
can be used (e.g. a function, a loop). During the execution of a program, a
variable cannot be accessed from outside of its scope. This effectively limits
the visibility of the variable, which prevents its value from being read or
written in other parts of the code.

### Actions

Move the declaration to the smallest possible scope.

### Relevance

Explicitly declaring scalar variables in the smallest scope possible simplifies
data flow analysis, which may be difficult, time-consuming and error-prone
especially in big code bases that contain many function calls spread along
multiple files. It makes it easier for the compiler to track the usage of
variables. By minimizing the number of statements where the value can be
modified, it is easier to diagnose why a variable is taking an erroneous value.
Additionally, it reduces the likelihood of reusing variables for multiple,
incompatible purposes, making code testing significantly easier.

> [!NOTE]
> In the context of parallel programming, one of the biggest challenges is to do
> data scoping correctly and decide on how to manage all the variables of the
> code properly. For example, it is very important to determine if a variable
> needs to be declared as private to the threads or if it needs to be shared
> among all the threads. Not doing so may introduce race conditions during the
> parallel execution, which makes the result of the code wrong in unpredictable
> ways.

### Code examples

#### C

In the following code, the function `example` declares a variable `t` used in
each iteration of the loop to hold a value that is then assigned to the array
`result`. The variable `t` is not used outside of the loop.

```c
void example() {
  int t;
  int result[10];

  for (int i = 0; i < 10; i++) {
    t = i + 1;
    result[i] = t;
  }
}

```

In this code, the smallest possible scope for the variable `t` is within the
loop body. The resulting code would be as follows:

```c
void example() {
  int result[10];

  for (int i = 0; i < 10; i++) {
    int t = i + 1;
    result[i] = t;
  }
}
```

From the perspective of parallel programming, moving the declaration of variable
`t` to the smallest possible scope helps to prevent potential race conditions.
For example, in the OpenMP parallel implementation shown below there is no need
to use the clause `private(t)`, as the declaration scope of `t` inherently
dictates that it is private to each thread. This avoids potential race
conditions because each thread modifies its own copy of the variable `t`.

```c
void example() {
  int result[10];

  #pragma omp parallel for default(none) shared(result)
  for (int i = 0; i < 10; i++) {
    int t = i + 1;
    result[i] = t;
  }
}
```

#### Fortran

Fortran 2008 introduced the `BLOCK` construct. This feature allows to organize
code within larger programs by grouping sections together. Conveniently,
`BLOCK` supports new variable declarations within those sections.

In the following code, the subroutine `example` declares a variable `t` used in
each iteration of the loop to hold a value that is then assigned to the array
`result`. The variable `t` is not used outside of the loop.

```f90
subroutine example()
  implicit none
  integer :: t
  integer, dimension(10) :: result
  integer :: i

  do i = 1, 10
    t = i + 1
    result(i) = t
  end do
end subroutine example
```

However, the smallest possible scope for the variable `t` is within the loop
body. The resulting code with the `BLOCK` construct would be as follows:

```f90
subroutine example()
  implicit none
  integer, dimension(10) :: result
  integer :: i

  do i = 1, 10
    block
      integer :: t
      t = i + 1
      result(i) = t
    end block
  end do
end subroutine example
```

From the perspective of parallel programming, moving the declaration of
variable `t` to the smallest possible scope helps to prevent potential race
conditions. For example, in the OpenMP parallel implementation shown below
there is no need to use the clause `private(t)`, as the declaration scope of
`t` inherently dictates that it is private to each thread. This avoids
potential race conditions because each thread modifies its own copy of the
variable `t`.

```f90
subroutine example()
  implicit none
  integer, dimension(10) :: result
  integer :: i

  !$omp parallel do default(none) shared(result) private(i)
  do i = 1, 10
    block
      integer :: t
      t = i + 1
      result(i) = t
    end block
  end do
  !$omp end parallel do
end subroutine example
```

### Related resources

* [PWR002 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR002/)

### References

* [Scope of variables](https://users.cs.cf.ac.uk/Dave.Marshall/PERL/node52.html)
