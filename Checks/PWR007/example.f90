! PWR007: Disable implicit declaration of variables

subroutine example()
  ! NOT-PWR071: We`re using implicit typing on purpose
  num1 = 7
  num2 = 2.5 ! num2 is implicitly typed as integer
  res = num1 / num2 ! res = 3.0
end subroutine example
