// PWR031: Replace pow by multiplication, division and/or square root

#include <math.h>
#include <stdio.h>

__attribute__((const)) double raise_x_to_the_power_of_1_point_5(double x) {
  return x * sqrt(x);
}

int main() {
  printf("2 raised to the power of 1.5 is: %0.15f\n",
         raise_x_to_the_power_of_1_point_5(2.0));
  return 0;
}
