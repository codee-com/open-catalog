! PWR063: avoid using legacy Fortran constructs

      PROGRAM LegacyFortran
        INTEGER A, B, C, X, I
        COMMON /MyCommonBlock/ A, B, C
        DATA A /10/, B /20/, C /30/, X /0/, I /1/

10      CONTINUE
        CALL UpdateValue(X)
        WRITE(*,*) "Update X =", X
        I = I + 1
        IF (I - 11) 10, 20, 30

20      CONTINUE
        WRITE(*,*) "Final  X =", X
        STOP

30      CONTINUE
        WRITE(*,*) "Error: loop exceeded 10 iterations"
        STOP
      END PROGRAM LegacyFortran

      SUBROUTINE UpdateValue(X)
        INTEGER A, B, C, X
        COMMON /MyCommonBlock/ A, B, C
        X = X + A + B + C
      END SUBROUTINE UpdateValue
