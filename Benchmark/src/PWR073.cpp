#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void transform_vector_f(int n, double *X, double *result, double coeff_a,
                        double coeff_b);
void transform_vector_improved_f(int n, double *X, double *result,
                                 double coeff_a, double coeff_b);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;
constexpr double COEFF_A = 4.5;
constexpr double COEFF_B = 0.73;

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto X = OpenCatalog::CreateRandomVector<double>(N);
  auto result = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    transform_vector_f(N, X.data(), result.data(), COEFF_A, COEFF_B);
    benchmark::DoNotOptimize(result);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto X = OpenCatalog::CreateRandomVector<double>(N);
  auto result = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    transform_vector_improved_f(N, X.data(), result.data(), COEFF_A, COEFF_B);
    benchmark::DoNotOptimize(result);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested code
// modernization does not incur any performance penalty
OC_BENCHMARK("PWR073 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR073 Fortran Improved", FortranImprovedBench);

#endif
