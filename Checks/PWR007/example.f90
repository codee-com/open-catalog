! PWR007: Disable implicit declaration of variables

subroutine example()
  num1 = 7
  num2 = 2.5 ! num2 is implicitly typed as integer
  res = num1 / num2 ! res = 3.0
end subroutine example
