#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void compute_damped_sinusoid(const int n, const double *timesteps,
                             const double amplitude,
                             const double angularFrequency,
                             const double decayRate, const double phaseShift,
                             double *results);
void compute_damped_sinusoid_improved(const int n, const double *timesteps,
                                      const double amplitude,
                                      const double angularFrequency,
                                      const double decayRate,
                                      const double phaseShift, double *results);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;
constexpr double AMPLITUDE = 1.0;
constexpr double ANGULAR_FREQUENCY = 2.0;
constexpr double DECAY_RATE = 0.5;
constexpr double PHASE_SHIFT = 3.141592653589793 / 4;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  const auto timesteps = OpenCatalog::CreateRandomVector<double>(N, 0.0, 10.0);
  auto results = OpenCatalog::CreateUninitializedVector<double>(N);

  for (auto _ : state) {
    compute_damped_sinusoid(N, timesteps.data(), AMPLITUDE, ANGULAR_FREQUENCY,
                            DECAY_RATE, PHASE_SHIFT, results.data());
    benchmark::DoNotOptimize(results);
  }
}

static void CImprovedBench(benchmark::State &state) {
  const auto timesteps = OpenCatalog::CreateRandomVector<double>(N, 0.0, 10.0);
  auto results = OpenCatalog::CreateUninitializedVector<double>(N);

  for (auto _ : state) {
    compute_damped_sinusoid_improved(N, timesteps.data(), AMPLITUDE,
                                     ANGULAR_FREQUENCY, DECAY_RATE, PHASE_SHIFT,
                                     results.data());
    benchmark::DoNotOptimize(results);
  }
}

// A performance impact is expected when performing higher-precision
// calculations
OC_BENCHMARK("PWR037 C Example", CExampleBench);
OC_BENCHMARK("PWR037 C Fixed", CImprovedBench);

#endif
