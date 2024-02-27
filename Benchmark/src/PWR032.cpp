#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void example(const int N, const float *X1, const float *Y1, const float *X2,
             const float *Y2, float *result);
void solution(const int N, const float *X1, const float *Y1, const float *X2,
              const float *Y2, float *result);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 128 * 128 * 128;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  const auto &X1 = OpenCatalog::CreateRandomVector<float>(N);
  const auto &Y1 = OpenCatalog::CreateRandomVector<float>(N);

  const auto &X2 = OpenCatalog::CreateRandomVector<float>(N);
  const auto &Y2 = OpenCatalog::CreateRandomVector<float>(N);

  auto result = OpenCatalog::CreateUninitializedVector<float>(N);

  for (auto _ : state) {
    example(N, X1.data(), Y1.data(), X2.data(), Y2.data(), result.data());
    benchmark::DoNotOptimize(result);
  }
}

static void CImprovedBench(benchmark::State &state) {
  const auto &X1 = OpenCatalog::CreateRandomVector<float>(N);
  const auto &Y1 = OpenCatalog::CreateRandomVector<float>(N);

  const auto &X2 = OpenCatalog::CreateRandomVector<float>(N);
  const auto &Y2 = OpenCatalog::CreateRandomVector<float>(N);

  auto result = OpenCatalog::CreateUninitializedVector<float>(N);

  for (auto _ : state) {
    solution(N, X1.data(), Y1.data(), X2.data(), Y2.data(), result.data());
    benchmark::DoNotOptimize(result);
  }
}

OC_BENCHMARK("PWR032 C Example", CExampleBench);
OC_BENCHMARK("PWR032 C Improved", CImprovedBench);

#endif
