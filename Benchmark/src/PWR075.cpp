#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void calculate_distances_f(int n, double *P1, double *P2, double *radiuses,
                           double *distances);
void calculate_distances_improved_f(int n, double *P1, double *P2,
                                    double *radiuses, double *distances);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 64;

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto P1 = OpenCatalog::CreateRandomVector<double>(N, /*lowerBound=*/0.0,
                                                    /*upperBound=*/360.0);
  auto P2 = OpenCatalog::CreateRandomVector<double>(N, /*lowerBound=*/0.0,
                                                    /*upperBound=*/360.0);
  auto radiuses = OpenCatalog::CreateRandomVector<double>(N);
  auto distances = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    calculate_distances_f(N, P1.data(), P2.data(), radiuses.data(),
                          distances.data());
    benchmark::DoNotOptimize(distances);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto P1 = OpenCatalog::CreateRandomVector<double>(N, /*lowerBound=*/0.0,
                                                    /*upperBound=*/360.0);
  auto P2 = OpenCatalog::CreateRandomVector<double>(N, /*lowerBound=*/0.0,
                                                    /*upperBound=*/360.0);
  auto radiuses = OpenCatalog::CreateRandomVector<double>(N);
  auto distances = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    calculate_distances_improved_f(N, P1.data(), P2.data(), radiuses.data(),
                                   distances.data());
    benchmark::DoNotOptimize(distances);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested code
// modernization does not incur any performance penalty
OC_BENCHMARK("PWR075 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR075 Fortran Improved", FortranImprovedBench);

#endif
