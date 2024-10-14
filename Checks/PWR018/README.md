# PWR018: Call to recursive function within a loop inhibits vectorization

### Issue

Recursive calls within a loop inhibit
[vectorization](../../Glossary/Vectorization.md).

### Actions

Rewrite the loop without the recursive function call to enable vectorization.

### Relevance

Many loops can benefit from vectorization. However, loops with calls to
recursive functions cannot be vectorized. Recursive functions introduce complex
control flow logic which the compilers cannot vectorize automatically.

>**Note**  
>Whether the loop with a recursive function call is vectorizable or not after
>de-recursion depends on the property of the original recursive function itself.
>If the complex control flow logic remains after de-recursion, the loop will
>remain not vectorizable.

### Code example

#### C

In the following example, the loop is invoking a recursive function computing
the Fibonacci number. This recursion inhibits the vectorization of the loop:

```c
double fib(unsigned n) {
  if (n == 0) {
    return 0.0;
  }
  if (n == 1) {
    return 1.0;
  }
  return fib(n - 1) + fib(n - 2);
}

double example(unsigned times) {
  double sum = 0.0;
  for (unsigned i = 0; i < times; i++) {
    sum += fib(i);
  }
  return sum;
}
```

As an alternative, Fibonacci's sequence can be calculated non-recursively:

```c
double example(unsigned times) {
  double sum = 0.0;
  double fib_0 = 0.0;
  double fib_1 = 1.0;
  for (unsigned i = 2; i < times; i++) {
    double fib = fib_0 + fib_1;
    sum += fib;
    fib_0 = fib_1;
    fib_1 = fib;
  }
  return sum;
}
```

#### Fortran

In the following example, the loop is invoking a recursive function computing
the Fibonacci number. This recursion inhibits the vectorization of the loop:

```f90
module mod_fibonacci
  contains
  recursive function fibonacci(n) result(fibo)
    implicit none
    integer, intent(in) :: n
    integer :: fibo

    if (n == 0) then
      fibo = 0
    else if (n == 1) then
      fibo = 1
    else
      fibo = fibonacci(n - 1) + fibonacci(n - 2)
    end if
  end function fibonacci
end module mod_fibonacci

subroutine example(times)
  use mod_fibonacci, only : fibonacci

  implicit none
  integer, intent(in) :: times
  integer :: i, sum

  sum = 0

  do i = 0, times - 1
    sum = sum + fibonacci(i)
  end do
end subroutine example
```

As an alternative, Fibonacci's sequence can be calculated non-recursively:

```f90
subroutine example(times)
  implicit none
  integer, intent(in) :: times
  integer :: i, sum
  integer :: fib_0, fib_1, fib

  sum = 0
  fib_0 = 0
  fib_1 = 1

  do i = 2, times - 1
    fib = fib_0 + fib_1
    sum = sum + fib
    fib_0 = fib_1
    fib_1 = fib
  end do
end subroutine example
```

### Related resources

* [PWR018 examples](../PWR018)

### References

* [Vectorization](../../Glossary/Vectorization.md)

* [OpenMP canonical form](../../Glossary/OpenMP-canonical-form.md)
