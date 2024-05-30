// PWR003: Explicitly declare pure functions

#include "gravity_pure.h"

// Computes the weight of each object in a vector
void solution(const int N, const double *masses, double *weights) {
  for (int i = 0; i < N; ++i) {
    weights[i] = masses[i] * gravity(3);
  }
}
