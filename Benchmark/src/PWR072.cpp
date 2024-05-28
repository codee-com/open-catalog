#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
double compute_final_moving_average_f(int n, double *X);
double compute_final_moving_average_improved_f(int n, double *X);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto X = OpenCatalog::CreateRandomVector<double>(N);
  double result = 0.0;

  for (auto _ : state) {
    result = compute_final_moving_average_f(N, X.data());
    benchmark::DoNotOptimize(result);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto X = OpenCatalog::CreateRandomVector<double>(N);
  double result = 0.0;

  for (auto _ : state) {
    result = compute_final_moving_average_improved_f(N, X.data());
    benchmark::DoNotOptimize(result);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested code
// modernization does not incur any performance penalty
OC_BENCHMARK("PWR072 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR072 Fortran Improved", FortranImprovedBench);

#endif
