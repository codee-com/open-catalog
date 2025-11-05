#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
double compute_stats_f(const int n, const double *data, double *mean,
                       double *stddev);
double compute_stats_improved_f(const int n, const double *data, double *mean,
                                double *stddev);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double mean, stddev;
    compute_stats_f(N, array.data(), &mean, &stddev);
    benchmark::DoNotOptimize(mean);
    benchmark::DoNotOptimize(stddev);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double mean, stddev;
    compute_stats_improved_f(N, array.data(), &mean, &stddev);
    benchmark::DoNotOptimize(mean);
    benchmark::DoNotOptimize(stddev);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested check does
// not incur any performance penalty
OC_BENCHMARK("PWR081 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR081 Fortran Improved", FortranImprovedBench);

#endif
