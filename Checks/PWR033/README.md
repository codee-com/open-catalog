# PWR033: Move invariant conditional out of the loop to avoid redundant computations

### Issue

[Loop-invariant conditional](../../Glossary/Loop-unswitching.md) can be moved out of
the loop to save computations.

### Action

Move the invariant conditional out of the loop.

### Relevance

A conditional is said to be invariant when it evaluates to the same value
(either true or false) for all the iterations of a loop. By that definition,
there is no need to repeat the evaluation of the condition in each iteration
since it will not change from one iteration to another. Therefore, those
computations required for the evaluation can be saved by extracting the
conditional out of the loop. In some cases this will result in duplicating the
loop for each possible result of the conditional. In these scenarios, the code
may be larger but it should also become faster.

> [!NOTE]
> This optimization is called [loop unswitching](../../Glossary/Loop-unswitching.md)
> and the compilers can do it automatically in simple cases. However, in more
> complex cases, the compiler will omit this optimization and therefore it is
> beneficial to do it manually..

### Code example

#### C

The following loop increments a variable by 1 or 2 depending on the function
argument:

```c
void example(int addTwo) {
  int sum = 0;
  for (int i = 0; i < 1000; i++) {
    sum += addTwo ? 2 : 1;
  }
}
```

In each iteration, the increment statement evaluates the argument to decide how
much to increment. However, this value is fixed for the whole execution of the
function and thus, the conditional can be moved outside of the loop. The
resulting code is as follows:

```c
void example(int addTwo) {
  int sum = 0;
  int add = addTwo ? 2 : 1
  for (int i = 0; i < 1000; i++) {
    sum += add;
  }
}
```

#### Fortran

The following loop increments a variable by 1 or 2 depending on the function
argument:

```fortran
subroutine example(addTwo)
  implicit none
  logical, intent(in) :: addTwo
  integer :: i, sum

  do i = 1, 1000
    if (addTwo .eqv. .true.) then
      sum = sum + 2
    else
      sum = sum + 1
    end if
  end do
end subroutine
```

In each iteration, the increment statement evaluates the argument to decide how
much to increment. However, this value is fixed for the whole execution of the
function and thus, the conditional can be moved outside of the loop. The
resulting code is as follows:

```fortran
subroutine example(addTwo)
  implicit none
  logical, intent(in) :: addTwo
  integer :: add, i, sum

  if (addTwo .eqv. .true.) then
    add = 2
  else
    add = 1
  end if

  do i = 1, 1000
    sum = sum + add
  end do
end subroutine
```

### Related resources

* [PWR033 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR033/)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
