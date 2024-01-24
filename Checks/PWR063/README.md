# PWR063: Avoid using legacy Fortran constructs

### Issue

Legacy Fortran constructs can be detrimental to code readability,
maintainability, and performance.

### Relevance

Fortran has undergone numerous revisions since its debut in the 1950s, with
Fortran 2023 as the most recent one. Each iteration has introduced more powerful
features to enhance productivity, while also addressing the shortcomings of
previous versions. A notable example is the once indispensable `GO TO`
statement. This feature was crucial for handling program flow in the
pre-structured programming era, but its usage is now heavily discouraged in
favor of newer and more explicit constructs, like `IF`/`ELSE` and `CASE`.

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

  * Use `MODULE` structures instead of `COMMON` blocks for better organization
  and modularity.

  * Prefer `REAL` over `DOUBLE PRECISION` for a more standardized data type.

  * Choose modern character types over Hollerith Constants.

  * Use pointers or derived types rather than the `EQUIVALENCE` statement to
  declare references to the same memory location.

  * Avoid implicit array shape changes in subroutine calls.

* For clearer program flow control:

  * Use `CASE` constructs instead of assigned or computed `GO TO` statements.

  * Replace arithmetic `IF` statements with block `IF` constructs.

  * Ensure that any branch to and `END IF` statement is within the corresponding
  block `IF`.

  * Use only integer control variables in `DO` loops.

  * Replace `PAUSE` with contemporary mechanisms.

  * Limit `RETURN` statements to have a single exit point.

  * Avoid alternate `RETURN` statements.

* Other outdated or less efficient statements to avoid include:

  * `ASSIGN`.

  * `BACKSPACE`.

  * Blank `COMMON`.

  * `BLOCK DATA`.

  * `DATA`.

  * Labeled `DO`.

### Actions

Replace the identified legacy construct with modern Fortran statements, while
preserving the original functionality of the code.

### Code examples

The following examples demonstrate how to enhance code readability and structure
by eliminating legacy Fortran constructs. This aids programmers in understanding
and maintaining their code, while also enabling compilers to apply performance
optimizations more effectively.

#### Arithmetic `IF`

The following example demonstrates a loop that iterates from 1 to 10 using an
arithmetic `IF` statement:

```f90
      PROGRAM ArithmeticIf
        INTEGER I, X(10)
        I = 1

10      CONTINUE
        X(I) = I * 10
        WRITE(*,*) "Update X =", X
        I = I + 1
        IF (I - 11) 10, 20, 30

20      CONTINUE
        WRITE(*,*) "Final X = ", X
        STOP

30      CONTINUE
        WRITE(*,*) "Error: out of bounds!"
        STOP
      END PROGRAM ArithmeticIf
```

Although it is a simple program, using an arithmetic `IF` to drive the flow of
the loop makes the behaviour of the program less explicit than modern loop
construct.

We may improve the readability, intent, and maintainability of the code if we
use a modern `DO` loop construct:

```f90
      PROGRAM DoLoop
        INTEGER I, X(10)

        DO I = 1, 10
          X(I) = I * 10
          WRITE(*,*) "Update X =", X
        END DO

        WRITE(*, *) "Final X =", X
      END PROGRAM DoLoop
```

This construct provides a straightforward and safer iteration control mechanism,
clearly stating its "jumps" and stop conditions.

#### Using `COMMON` and `DATA` constructs

The following program demonstrates three global variables (i.e., `A`, `B`, `C`)
that are shared between the main program and a subroutine using the `COMMON`
construct and are initialized out of line using the `DATA` construct:

```f90
      PROGRAM CommonDataConstructs
        INTEGER A, B, C, I
        COMMON /CommonBlock/ A, B, C
        DATA A /10/, B /20/, C /30/

        DO I = 1, 5
          CALL UpdateValues(I)
          WRITE(*,*) "Update A, B, and C", A, B, C
        END DO

        WRITE(*,*) "Final A, B, and C", A, B, C
      END PROGRAM CommonDataConstructs

      SUBROUTINE UpdateValues(X)
        INTEGER A, B, C, X
        COMMON /CommonBlock/ A, B, C

        A = A + X
        B = B * X
        C = C + A + B
      END SUBROUTINE UpdateValues
```

Again, although the program is simple, the `COMMON` construct makes it harder to
reason about the state of the variables `A`, `B`, and `C` and creates a hidden
dependency between the main program and the subroutine that may make the code
more prone to errors during modifications. Moreover, variables are initialized
separately from their declarations using the `DATA` construct, thus reducing
intent and clarity.

The code above may be improved if we use the `MODULE` construct and declare and
initialize variables simultaneously:

```f90
      MODULE MyModule
        IMPLICIT NONE
        INTEGER :: A = 10, B = 20, C = 30

      CONTAINS
        SUBROUTINE UpdateValues(X)
          IMPLICIT NONE
          INTEGER :: X

          A = A + X
          B = B * X
          C = C + A + B
        END SUBROUTINE UpdateValues
      END MODULE MyModule

      PROGRAM ModernExample
        USE MyModule

        IMPLICIT NONE
        INTEGER :: I

        DO I = 1, 5
          CALL UpdateValues(I)
          WRITE(*,*) "Update A, B, and C", A, B, C
        END DO

        WRITE(*,*) "Final A, B, and C", A, B, C
      END PROGRAM ModernExample
```

This alternative program is clearer and benefits from the encapsulation provided
by the `MODULE` construct. The subroutine and its related variables are now
stored together within a `MODULE`, clearly stating their relationship.

### Related resources

* [PWR063 source code examples](/Checks/PWR063)

### References

* ["Fortran 90/95 Coding Conventions", Jack Carlson and Olaf David, Object Modeling System (OMS) Laboratory, Department of Civil and Environmental Engineering, Colorado State University](https://alm.engr.colostate.edu/cb/wiki/16983)
[last checked January 2024]
