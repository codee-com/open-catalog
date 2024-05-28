#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void clamp_even_data_points_f(int n, double *X, int min_value, int max_value);
void clamp_even_data_points_improved_f(int n, double *X, int min_value,
                                       int max_value);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;
constexpr double MIN_VALUE = 2.71;
constexpr double MAX_VALUE = 3.14;

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto X = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    clamp_even_data_points_f(N, X.data(), MIN_VALUE, MAX_VALUE);
    benchmark::DoNotOptimize(X);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto X = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    clamp_even_data_points_improved_f(N, X.data(), MIN_VALUE, MAX_VALUE);
    benchmark::DoNotOptimize(X);
  }
}

OC_BENCHMARK("PWR070 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR070 Fortran Improved", FortranImprovedBench);

#endif
