// PWR003: Explicitly declare pure functions

#ifdef __GNUC__
#define CONST_FUNCTION __attribute__((const))
#else
#define CONST_FUNCTION
#endif

CONST_FUNCTION extern double gravity(const int planetIdx);

// Computes the weight of each object in a vector
void solution(const int N, const double *masses, double *weights) {
  for (int i = 0; i < N; ++i) {
    weights[i] = masses[i] * gravity(3);
  }
}
