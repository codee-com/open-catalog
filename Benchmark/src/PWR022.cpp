#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void example(int n, const double *A, const double *B, const double *C,
             double *D);
void solution(int n, const double *A, const double *B, const double *C,
              double *D);
void example_f(int n, double *A, double *B, double *C, double *D);
void solution_f(int n, double *A, double *B, double *C, double *D);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 128;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  auto A = OpenCatalog::CreateRandomVector<double>(N);
  auto B = OpenCatalog::CreateRandomVector<double>(N * N);
  auto C = OpenCatalog::CreateRandomVector<double>(N * N * N);
  auto D = OpenCatalog::CreateUninitializedVector<double>(N * N);
  for (auto _ : state) {
    example(N, A.data(), B.data(), C.data(), D.data());
    benchmark::DoNotOptimize(D);
  }
}

static void CImprovedBench(benchmark::State &state) {
  auto A = OpenCatalog::CreateRandomVector<double>(N);
  auto B = OpenCatalog::CreateRandomVector<double>(N * N);
  auto C = OpenCatalog::CreateRandomVector<double>(N * N * N);
  auto D = OpenCatalog::CreateUninitializedVector<double>(N * N);
  for (auto _ : state) {
    solution(N, A.data(), B.data(), C.data(), D.data());
    benchmark::DoNotOptimize(D);
  }
}

OC_BENCHMARK("PWR022 C Example", CExampleBench);
OC_BENCHMARK("PWR022 C Improved", CImprovedBench);

#endif

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto A = OpenCatalog::CreateRandomVector<double>(N);
  auto B = OpenCatalog::CreateRandomVector<double>(N * N);
  auto C = OpenCatalog::CreateRandomVector<double>(N * N * N);
  auto D = OpenCatalog::CreateUninitializedVector<double>(N * N);
  for (auto _ : state) {
    example_f(N, A.data(), B.data(), C.data(), D.data());
    benchmark::DoNotOptimize(D);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto A = OpenCatalog::CreateRandomVector<double>(N);
  auto B = OpenCatalog::CreateRandomVector<double>(N * N);
  auto C = OpenCatalog::CreateRandomVector<double>(N * N * N);
  auto D = OpenCatalog::CreateUninitializedVector<double>(N * N);
  for (auto _ : state) {
    solution_f(N, A.data(), B.data(), C.data(), D.data());
    benchmark::DoNotOptimize(D);
  }
}

OC_BENCHMARK("PWR022 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR022 Fortran Improved", FortranImprovedBench);

#endif
