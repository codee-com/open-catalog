// PWR046: Replace two divisions with a division and a multiplication

// Computes the harmonic means for each pair {x_i, y_i}
//
// https://en.wikipedia.org/wiki/Harmonic_mean
void compute_harmonic_mean_between_pairs(int n, const double *x,
                                         const double *y, double *result) {
  for (int i = 0; i < n; ++i) {
    result[i] = 2 / (1 / x[i] + 1 / y[i]);
  }
}
