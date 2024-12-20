#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
double midpoint_rule_x_pow_1_5_sin_x(double a, double b, int n);
double midpoint_rule_x_pow_1_5_sin_x_improved(double a, double b, int n);
double midpoint_rule_x_pow_1_5_sin_x_f(double a, double b, int n);
double midpoint_rule_x_pow_1_5_sin_x_improved_f(double a, double b, int n);
}

constexpr double a = 20.0;
constexpr double b = 30.0;
constexpr int n = 524288; // Adjusted size to fit on microseconds

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  for (auto _ : state) {
    double result = midpoint_rule_x_pow_1_5_sin_x(a, b, n);
    benchmark::DoNotOptimize(result);
  }
}

static void CImprovedBench(benchmark::State &state) {
  for (auto _ : state) {
    double result = midpoint_rule_x_pow_1_5_sin_x_improved(a, b, n);
    benchmark::DoNotOptimize(result);
  }
}

OC_BENCHMARK("PWR031 C Example", CExampleBench);
OC_BENCHMARK("PWR031 C Improved", CImprovedBench);

#endif

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  for (auto _ : state) {
    double result = midpoint_rule_x_pow_1_5_sin_x_f(a, b, n);
    benchmark::DoNotOptimize(result);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  for (auto _ : state) {
    double result = midpoint_rule_x_pow_1_5_sin_x_improved_f(a, b, n);
    benchmark::DoNotOptimize(result);
  }
}

OC_BENCHMARK("PWR031 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR031 Fortran Improved", FortranImprovedBench);

#endif
