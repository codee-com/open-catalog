#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void example(const int N, const double *masses, double *weights);
void solution(const int N, const double *masses, double *weights);
void example_f(int N, double *masses, double *weights);
void solution_f(int N, double *masses, double *weights);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 128 * 128 * 128;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  const auto masses = OpenCatalog::CreateRandomVector<double>(N);
  auto weights = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    example(N, masses.data(), weights.data());
    benchmark::DoNotOptimize(weights);
  }
}

static void CImprovedBench(benchmark::State &state) {
  const auto masses = OpenCatalog::CreateRandomVector<double>(N);
  auto weights = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    solution(N, masses.data(), weights.data());
    benchmark::DoNotOptimize(weights);
  }
}

OC_BENCHMARK("PWR003 C Example", CExampleBench);
OC_BENCHMARK("PWR003 C Improved", CImprovedBench);

#endif

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto masses = OpenCatalog::CreateRandomVector<double>(N);
  auto weights = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    example_f(N, masses.data(), weights.data());
    benchmark::DoNotOptimize(weights);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto masses = OpenCatalog::CreateRandomVector<double>(N);
  auto weights = OpenCatalog::CreateZeroVector<double>(N);

  for (auto _ : state) {
    solution_f(N, masses.data(), weights.data());
    benchmark::DoNotOptimize(weights);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested code
// modernization does not incur any performance penalty
OC_BENCHMARK("PWR003 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR003 Fortran Improved", FortranImprovedBench);

#endif
