// PWR031: Replace pow by multiplication, division and/or square root

#include <math.h>

// Computes the integral of $f(x) = x^{1.5} \sin(x)$ over the interval
// $\{a, b\}$ using the midpoint rule with $n$ samples
__attribute__((const)) double midpoint_rule_x_pow_1_5_sin_x(double a, double b,
                                                            int n) {
  double integral = 0.0;
  double dx = (b - a) / n;
  for (int i = 0; i < n; ++i) {
    double x = fma((i + 0.5), dx, a);
    integral = fma(pow(x, 1.5) * sin(x), dx, integral);
  }
  return integral;
}
