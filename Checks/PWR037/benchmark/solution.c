// PWR037: Potential precision loss in call to mathematical function

#include <math.h>

// https://en.wikipedia.org/wiki/Damping#Damped_sine_wave
void compute_damped_sinusoid_improved(const int n, const double *timesteps,
                                      const double amplitude,
                                      const double angularFrequency,
                                      const double decayRate,
                                      const double phaseShift,
                                      double *results) {

  for (int i = 0; i < n; ++i) {
    double exponentialDecay = exp(-decayRate * timesteps[i]);
    double angle = angularFrequency * timesteps[i] + phaseShift;
    results[i] = amplitude * exponentialDecay * cos(angle);
  }
}
