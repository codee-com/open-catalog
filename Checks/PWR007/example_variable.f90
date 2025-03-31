! PWR007: Disable the implicit declaration of variables and procedures

program example
  ! NOT-PWR071: We`re using implicit typing on purpose
  num1 = 7
  num2 = 2.5 ! num2 is implicitly typed as integer
  res = num1 / num2
  print *, res
end program example
