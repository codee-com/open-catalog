# PWR085: Favor iterative implementations over recursion to prevent stack overflows

### Issue

Uncontrolled recursive calls may lead to program crashes.

### Actions

Rewrite the function using an iterative approach to avoid exhausting stack
memory.

### Relevance

Recursive procedures can be the most natural implementation of certain
algorithms. However, in general, each recursive call will require its own stack
frame; which may lead to unbounded stack growth and, eventually, overflow.

When the recursive call is the last action of the procedure, the caller's frame
can be replaced with that of the callee, conserving stack space. However, this
tail-call optimization is not required by the C, C++ or Fortran standards.

Therefore even in these cases an iterative approach is required to reliably
prevent uncontrolled stack growth, which can be fatal in a variety and a mix of
scenarios, such as:
- A system with limited stack memory.
- An algorithm that requires a high number of recursive iterations.
- A badly implemented control condition that does not exit when needed or when
the recursion becomes too deep.

> [!TIP]
> In addition to increasing code resilience, rewriting algorithms
> in a non-recursive way facilitates vectorization. See check
> [PWR018](../PWR018/README.md) for more details.

### Code example

#### C

In the following example, the loop is invoking a recursive function computing
the Fibonacci number:

```c
// example.c
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
// solution.c
__attribute__((pure)) double solution(unsigned times) {
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
the Fibonacci number:

```fortran
! example.f90
module mod_fibonacci
  implicit none
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

```fortran
! solution.f90
subroutine solution(times)
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
end subroutine solution
```

### Related resources

* [PWR085 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR085/)

### References

* [CWE - CWE-674: Uncontrolled Recursion](https://cwe.mitre.org/data/definitions/674.html)
[last checked February 2026]

* [Tail call optimization](https://en.wikipedia.org/wiki/Tail_call)
[last checked February 2026]
