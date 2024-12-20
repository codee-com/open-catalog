// PWR046: Replace two divisions with a division and a multiplication

// Computes the harmonic means for each pair {x_i, y_i}
//
// https://en.wikipedia.org/wiki/Harmonic_mean
void compute_harmonic_mean_between_pairs_improved(int n, const double *x,
                                                  const double *y,
                                                  double *result) {
  for (int i = 0; i < n; ++i) {
    // 1/x + 1/y = (x+y) / (xy)
    result[i] = (2 * x[i] * y[i]) / (x[i] + y[i]);
  }
}
