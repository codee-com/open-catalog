! PWR063: avoid using legacy Fortran constructs

      program LegacyFortran
        implicit none
        integer A, B, C, X, I
        ! NOT-PWR073: We're using the `common block` as a legacy construct
        common /MyCommonBlock/ A, B, C
        data A /10/, B /20/, C /30/, X /0/, I /1/

10      continue
        call UpdateValue(X)
        write(*,*) "Update X =", X
        I = I + 1
        if (I - 11) 10, 20, 30

20      continue
        write(*,*) "Final  X =", X
        stop

30      continue
        write(*,*) "Error: loop exceeded 10 iterations"
        stop

        contains

        subroutine UpdateValue(X)
          implicit none
          integer A, B, C
          integer, intent(inout) :: X
          common /MyCommonBlock/ A, B, C
          X = X + A + B + C
        end subroutine UpdateValue
      end program LegacyFortran
