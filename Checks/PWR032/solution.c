// PWR032: Avoid calls to mathematical functions with higher precision than
// required

#include <math.h>

void solution(const int N, const float *X1, const float *Y1, const float *X2,
              const float *Y2, float *result) {
  for (int i = 0; i < N; ++i) {
    const float x = X1[i] - X2[i];
    const float y = Y1[i] - Y2[i];
    result[i] = sqrtf(powf(x, 2) + powf(y, 2));
  }
}
