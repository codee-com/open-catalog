#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void calculate_euclidean_distances_f(int n, double *X1, double *Y1, double *X2,
                                     double *Y2, double *distances);
void calculate_euclidean_distances_improved_f(int n, double *X1, double *Y1,
                                              double *X2, double *Y2,
                                              double *distances);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto X1 = OpenCatalog::CreateRandomVector<double>(N);
  auto Y1 = OpenCatalog::CreateRandomVector<double>(N);
  auto X2 = OpenCatalog::CreateRandomVector<double>(N);
  auto Y2 = OpenCatalog::CreateRandomVector<double>(N);
  auto distances = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    calculate_euclidean_distances_f(N, X1.data(), Y1.data(), X2.data(),
                                    Y2.data(), distances.data());
    benchmark::DoNotOptimize(distances);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto X1 = OpenCatalog::CreateRandomVector<double>(N);
  auto Y1 = OpenCatalog::CreateRandomVector<double>(N);
  auto X2 = OpenCatalog::CreateRandomVector<double>(N);
  auto Y2 = OpenCatalog::CreateRandomVector<double>(N);
  auto distances = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    calculate_euclidean_distances_improved_f(N, X1.data(), Y1.data(), X2.data(),
                                             Y2.data(), distances.data());
    benchmark::DoNotOptimize(distances);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested code
// modernization does not incur any performance penalty
OC_BENCHMARK("PWR068 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR068 Fortran Improved", FortranImprovedBench);

#endif
