// PWR032: Avoid calls to mathematical functions with higher precision than
// required

#include <math.h>

void example(float x) {
  float res = sin(x);
}
