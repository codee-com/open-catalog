# PWR063: Avoid using legacy Fortran constructs

### Issue

Legacy Fortran constructs can be detrimental to code readability,
maintainability, and performance.

### Actions

Replace the identified legacy construct with modern Fortran statements, while
preserving the original functionality of the code.

### Relevance

Fortran has undergone numerous revisions since its debut in the 1950s, with
Fortran 2023 as the most recent one. Each iteration has introduced more powerful
features to enhance productivity, while also addressing the shortcomings of
previous versions. A notable example is the once indispensable `go to`
statement. This feature was crucial for handling program flow in the
pre-structured programming era, but its usage is now heavily discouraged in
favor of newer and more explicit constructs, like `if`/`else` and `case`.

In software development, it's common to reuse old source code due to its
time-tested reliability. Hence, many modern codebases integrate legacy
components. While valuable, their use of legacy Fortran constructs often makes
it harder to understand the program flow, increasing the risk of errors during
the maintenance or extension of old code. Moreover, this lack of clarity
challenges modern Fortran compilers, as their ability to optimize code is
hindered when facing complex program flows that are further obscured by the
heavy usage of legacy constructs.

Programming languages typically label outdated features as "deprecated", rather
than removing them entirely. This approach presents a significant challenge in
manually identifying legacy constructs in codebases. Fortran compilers might not
always flag these elements as problematic, as they remain compilable, or they
might only generate subtle deprecation warnings that are easily missed in large
project compilations.

Among current best practices for modernizing Fortran code, we can find:

* In terms of data storage:

  * Use `module` structures instead of `common` blocks for better organization
  and modularity.

  * Prefer `real` over `double precision` for a more standardized data type.

  * Choose modern character types over Hollerith Constants.

  * Use pointers or derived types rather than the `equivalence` statement to
  declare references to the same memory location.

  * Avoid implicit array shape changes in subroutine calls.

* For clearer program flow control:

  * Use `case` constructs instead of assigned or computed `go to` statements.

  * Replace arithmetic `if` statements with block `if` constructs.

  * Ensure that any branch to and `end if` statement is within the corresponding
  block `if`.

  * Use only integer control variables in `do` loops.

  * Replace `pause` with contemporary mechanisms.

  * Limit `return` statements to have a single exit point.

  * Avoid alternate `return` statements.

* Other outdated or less efficient statements to avoid include:

  * `assign`.

  * `backspace`.

  * Blank `common`.

  * `block data`.

  * `data`.

  * Labeled `do`.

### Code examples

The following examples demonstrate how to enhance code readability and structure
by eliminating legacy Fortran constructs. This aids programmers in understanding
and maintaining their code, while also enabling compilers to apply performance
optimizations more effectively.

#### Arithmetic `if`

The following example demonstrates a loop that iterates from 1 to 10 using an
arithmetic `if` statement:

```f90
      program ArithmeticIf
        implicit none
        integer I, X(10)
        I = 1

10      continue
        X(I) = I * 10
        write(*,*) "Update X =", X(:I)
        I = I + 1
        if (I - 11) 10, 20, 30

20      continue
        write(*,*) "Final X = ", X
        stop

30      continue
        write(*,*) "Error: out of bounds!"
        stop
      end program ArithmeticIf
```

Although it is a simple program, using an arithmetic `if` to drive the flow of
the loop makes the behaviour of the program less explicit than modern loop
construct.

We may improve the readability, intent, and maintainability of the code if we
use a modern `do` loop construct:

```f90
program DoLoop
  implicit none
  integer :: I, X(10)

  do I = 1, 10
    X(I) = I * 10
    write(*,*) "Update X =", X(:I)
  end do

  write(*, *) "Final X =", X
end program DoLoop
```

This construct provides a straightforward and safer iteration control mechanism,
clearly stating its "jumps" and stop conditions.

#### Using `common` and `data` constructs

The following program demonstrates three global variables (i.e., `A`, `B`, `C`)
that are shared between the main program and a subroutine using the `common`
construct and are initialized out of line using the `data` construct:

```f90
      program CommonDataConstructs
        implicit none
        integer A, B, C, I
        common /MyCommonBlock/ A, B, C
        data A /10/, B /20/, C /30/

        do I = 1, 5
          call UpdateValues(I)
          write(*,*) "Update A, B, and C", A, B, C
        end do

        write(*,*) "Final A, B, and C", A, B, C
      end program CommonDataConstructs

      subroutine UpdateValues(X)
        implicit none
        integer A, B, C, X
        common /MyCommonBlock/ A, B, C

        A = A + X
        B = B * X
        C = C + A + B
      end subroutine UpdateValues
```

Again, although the program is simple, the `common` construct makes it harder to
reason about the state of the variables `A`, `B`, and `C` and creates a hidden
dependency between the main program and the subroutine that may make the code
more prone to errors during modifications. Moreover, variables are initialized
separately from their declarations using the `data` construct, thus reducing
intent and clarity.

The code above may be improved if we use the `module` construct and declare and
initialize variables simultaneously:

```f90
module MyModule
  implicit none
  integer :: A = 10, B = 20, C = 30

contains
  subroutine UpdateValues(X)
    implicit none
    integer :: X

    A = A + X
    B = B * X
    C = C + A + B
  end subroutine UpdateValues
end module MyModule

program ModernExample
  use MyModule, only : A, B, C, UpdateValues

  implicit none
  integer :: I

  do I = 1, 5
    call UpdateValues(I)
    write(*,*) "Update A, B, and C", A, B, C
  end do

  write(*,*) "Final A, B, and C", A, B, C
end program ModernExample
```

This alternative program is clearer and benefits from the encapsulation provided
by the `module` construct. The subroutine and its related variables are now
stored together within a `module`, clearly stating their relationship.

### Related resources

* [PWR063 examples](../PWR063/)

### References

* ["Fortran 90/95 Coding Conventions"](https://alm.engr.colostate.edu/cb/wiki/16983#section-Fortran+Features+that+are+obsolescent+and_2For+discouraged),
Jack Carlson and Olaf David, Object Modeling System (OMS) Laboratory, Department
of Civil and Environmental Engineering, Colorado State University. See section
"Fortran Features that are obsolescent and/or discouraged". [last checked
January 2024]
