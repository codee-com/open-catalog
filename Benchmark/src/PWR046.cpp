#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
double compute_harmonic_mean_between_pairs(int n, const double *x,
                                           const double *y, double *results);
double compute_harmonic_mean_between_pairs_improved(int n, const double *x,
                                                    const double *y,
                                                    double *results);
double compute_harmonic_mean_between_pairs_f(int n, const double *x,
                                             const double *y, double *results);
double compute_harmonic_mean_between_pairs_improved_f(int n, const double *x,
                                                      const double *y,
                                                      double *results);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  auto x = OpenCatalog::CreateRandomVector<double>(N);
  auto y = OpenCatalog::CreateRandomVector<double>(N);
  auto results = OpenCatalog::CreateUninitializedVector<double>(N);

  for (auto _ : state) {
    compute_harmonic_mean_between_pairs(N, x.data(), y.data(), results.data());
    benchmark::DoNotOptimize(results);
  }
}

static void CImprovedBench(benchmark::State &state) {
  auto x = OpenCatalog::CreateRandomVector<double>(N);
  auto y = OpenCatalog::CreateRandomVector<double>(N);
  auto results = OpenCatalog::CreateUninitializedVector<double>(N);

  for (auto _ : state) {
    compute_harmonic_mean_between_pairs_improved(N, x.data(), y.data(),
                                                 results.data());
    benchmark::DoNotOptimize(results);
  }
}

OC_BENCHMARK("PWR046 C Example", CExampleBench);
OC_BENCHMARK("PWR046 C Improved", CImprovedBench);

#endif

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto x = OpenCatalog::CreateRandomVector<double>(N);
  auto y = OpenCatalog::CreateRandomVector<double>(N);
  auto results = OpenCatalog::CreateUninitializedVector<double>(N);

  for (auto _ : state) {
    compute_harmonic_mean_between_pairs_f(N, x.data(), y.data(),
                                          results.data());
    benchmark::DoNotOptimize(results);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto x = OpenCatalog::CreateRandomVector<double>(N);
  auto y = OpenCatalog::CreateRandomVector<double>(N);
  auto results = OpenCatalog::CreateUninitializedVector<double>(N);

  for (auto _ : state) {
    compute_harmonic_mean_between_pairs_improved_f(N, x.data(), y.data(),
                                                   results.data());
    benchmark::DoNotOptimize(results);
  }
}

OC_BENCHMARK("PWR046 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR046 Fortran Improved", FortranImprovedBench);

#endif
