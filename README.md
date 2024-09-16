# Open Catalog of Best Practices for Modernization and Optimization

## About

This Open Catalog is a collaborative effort to consolidate expert knowledge on
best practices for modernizing and optimizing code written in C, C++, and
Fortran programming languages. The Catalog consists of a comprehensive set of
checks (rules) that describe specific issues in the source code and provide
guidance on corrective actions, along with extensive documentation, example
codes and references to additional reading resources.

## Benchmarks

The Open Catalog includes [a suite of microbenchmarks](Benchmark/README.md)
designed to demonstrate:

- No performance degradation when implementing the modernization
  recommendations.
- Potential performance enhancements achievable through the optimization
  recommendations.

## Checks

| ID                       | Title                                                                                                                                                              | C | Fortran | C++ | AutoFix |
|:------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------ |:-:|:-------:|:---:|:-------:|
| [PWR001](Checks/PWR001/) | Declare global variables as function parameters                                                                                                                    | x | x       | x   |         |
| [PWR002](Checks/PWR002/) | Declare scalar variables in the smallest possible scope                                                                                                            | x |         | x   |         |
| [PWR003](Checks/PWR003/) | Explicitly declare pure functions                                                                                                                                  | x | x       | x   |         |
| [PWR004](Checks/PWR004/) | Declare OpenMP scoping for all variables                                                                                                                           | x | x       | x   |         |
| [PWR005](Checks/PWR005/) | Disable default OpenMP scoping                                                                                                                                     | x | x       | x   |         |
| [PWR006](Checks/PWR006/) | Avoid privatization of read-only variables                                                                                                                         | x | x       | x   |         |
| [PWR007](Checks/PWR007/) | Disable implicit declaration of variables                                                                                                                          |   | x       |     | 1       |
| [PWR008](Checks/PWR008/) | Declare the intent for each procedure parameter                                                                                                                    |   | x       |     | 1       |
| [PWR009](Checks/PWR009/) | Use OpenMP teams to offload work to GPU                                                                                                                            | x | x       | x   |         |
| [PWR010](Checks/PWR010/) | Avoid column-major array access in C/C++                                                                                                                           | x |         | x   |         |
| [PWR012](Checks/PWR012/) | Pass only required fields from derived type as parameters                                                                                                          | x | x       | x   |         |
| [PWR013](Checks/PWR013/) | Avoid copying unused variables to or from the GPU                                                                                                                  | x | x       | x   |         |
| [PWR014](Checks/PWR014/) | Out-of-dimension-bounds matrix access                                                                                                                              | x |         | x   |         |
| [PWR015](Checks/PWR015/) | Avoid copying unnecessary array elements to or from the GPU                                                                                                        | x | x       | x   |         |
| [PWR016](Checks/PWR016/) | Use separate arrays instead of an Array-of-Structs                                                                                                                 | x | x       | x   |         |
| [PWR017](Checks/PWR017/) | Using countable while loops instead of for loops may inhibit vectorization                                                                                         | x |         | x   |         |
| [PWR018](Checks/PWR018/) | Call to recursive function within a loop inhibits vectorization                                                                                                    | x | x       | x   |         |
| [PWR019](Checks/PWR019/) | Consider interchanging loops to favor vectorization by maximizing inner loop's trip count                                                                          | x | x       | x   |         |
| [PWR020](Checks/PWR020/) | Consider loop fission to enable vectorization                                                                                                                      | x | x       | x   |         |
| [PWR021](Checks/PWR021/) | Consider loop fission with scalar to vector promotion to enable vectorization                                                                                      | x | x       | x   |         |
| [PWR022](Checks/PWR022/) | Move invariant conditional out of the loop to facilitate vectorization                                                                                             | x | x       | x   |         |
| [PWR023](Checks/PWR023/) | Add 'restrict' for pointer function parameters to hint the compiler that vectorization is safe                                                                     | x |         | x   |         |
| [PWR024](Checks/PWR024/) | Loop can be rewritten in OpenMP canonical form                                                                                                                     | x |         | x   |         |
| [PWR025](Checks/PWR025/) | Consider annotating pure function with OpenMP 'declare simd'                                                                                                       | x | x       | x   |         |
| [PWR026](Checks/PWR026/) | Annotate function for OpenMP Offload                                                                                                                               | x | x       | x   |         |
| [PWR027](Checks/PWR027/) | Annotate function for OpenACC Offload                                                                                                                              | x | x       | x   |         |
| [PWR028](Checks/PWR028/) | Remove pointer increment preventing performance optimization                                                                                                       | x |         | x   |         |
| [PWR029](Checks/PWR029/) | Remove integer increment preventing performance optimization                                                                                                       | x | x       | x   |         |
| [PWR030](Checks/PWR030/) | Remove pointer assignment preventing performance optimization for perfectly nested loops                                                                           | x | x       | x   |         |
| [PWR031](Checks/PWR031/) | Replace call to pow by multiplication, division and/or square root                                                                                                 | x |         | x   |         |
| [PWR032](Checks/PWR032/) | Avoid calls to mathematical functions with higher precision than required                                                                                          | x |         | x   |         |
| [PWR033](Checks/PWR033/) | Move invariant conditional out of the loop to avoid redundant computations                                                                                         | x | x       | x   |         |
| [PWR034](Checks/PWR034/) | Avoid strided array access to improve performance                                                                                                                  | x | x       | x   |         |
| [PWR035](Checks/PWR035/) | Avoid non-consecutive array access to improve performance                                                                                                          | x | x       | x   |         |
| [PWR036](Checks/PWR036/) | Avoid indirect array access to improve performance                                                                                                                 | x | x       | x   |         |
| [PWR037](Checks/PWR037/) | Potential precision loss in call to mathematical function                                                                                                          | x |         | x   |         |
| [PWR039](Checks/PWR039/) | Consider loop interchange to improve the locality of reference and enable vectorization                                                                            | x | x       | x   | 1       |
| [PWR040](Checks/PWR040/) | Consider loop tiling to improve the locality of reference                                                                                                          | x | x       | x   |         |
| [PWR042](Checks/PWR042/) | Consider loop interchange by promoting the scalar reduction variable to an array                                                                                   | x | x       | x   |         |
| [PWR043](Checks/PWR043/) | Consider loop interchange by replacing the scalar reduction value                                                                                                  | x | x       | x   |         |
| [PWR044](Checks/PWR044/) | Avoid unnecessary floating-point data conversions involving constants                                                                                              | x |         | x   |         |
| [PWR045](Checks/PWR045/) | Replace division with a multiplication with a reciprocal                                                                                                           | x |         | x   |         |
| [PWR046](Checks/PWR046/) | Replace two divisions with a division and a multiplication                                                                                                         | x |         | x   |         |
| [PWR048](Checks/PWR048/) | Replace multiplication/addition combo with an explicit call to fused multiply-add                                                                                  | x |         | x   |         |
| [PWR049](Checks/PWR049/) | Move iterator-dependent condition outside of the loop                                                                                                              | x | x       | x   |         |
| [PWR050](Checks/PWR050/) | Consider applying multithreading parallelism to forall loop                                                                                                        | x | x       | x   | 1       |
| [PWR051](Checks/PWR051/) | Consider applying multithreading parallelism to scalar reduction loop                                                                                              | x | x       | x   | 1       |
| [PWR052](Checks/PWR052/) | Consider applying multithreading parallelism to sparse reduction loop                                                                                              | x | x       | x   | 1       |
| [PWR053](Checks/PWR053/) | Consider applying vectorization to forall loop                                                                                                                     | x | x       | x   | 1       |
| [PWR054](Checks/PWR054/) | Consider applying vectorization to scalar reduction loop                                                                                                           | x | x       | x   | 1       |
| [PWR055](Checks/PWR055/) | Consider applying offloading parallelism to forall loop                                                                                                            | x | x       | x   | 1       |
| [PWR056](Checks/PWR056/) | Consider applying offloading parallelism to scalar reduction loop                                                                                                  | x | x       | x   | 1       |
| [PWR057](Checks/PWR057/) | Consider applying offloading parallelism to sparse reduction loop                                                                                                  | x | x       | x   | 1       |
| [PWR060](Checks/PWR060/) | Consider loop fission to separate gather memory access pattern                                                                                                     | x | x       | x   |         |
| [PWR062](Checks/PWR062/) | Consider loop interchange by removing accumulation on array value                                                                                                  | x | x       | x   |         |
| [PWR063](Checks/PWR063/) | Avoid using legacy Fortran constructs                                                                                                                              |   | x       |     |         |
| [PWR068](Checks/PWR068/) | Encapsulate external procedures within modules to avoid the risks of calling implicit interfaces                                                                   |   | x       |     |         |
| [PWR069](Checks/PWR069/) | Use the keyword only to explicitly state what to import from a module                                                                                              |   | x       |     |         |
| [PWR070](Checks/PWR070/) | Declare array dummy arguments as assumed-shape arrays                                                                                                              |   | x       |     |         |
| [PWR071](Checks/PWR071/) | Prefer real(kind=kind_value) for declaring consistent floating types                                                                                               |   | x       |     |         |
| [PWR072](Checks/PWR072/) | Add an explicit save attribute when initializing variables in their declaration                                                                                    |   | x       |     |         |
| [PWR073](Checks/PWR073/) | Transform common block into a module for better data encapsulation                                                                                                 |   | x       |     |         |
| [PWR075](Checks/PWR075/) | Avoid using GNU Fortran extensions                                                                                                                                 |   | x       |     |         |
| [PWD002](Checks/PWD002/) | Unprotected multithreading reduction operation                                                                                                                     | x | x       | x   |         |
| [PWD003](Checks/PWD003/) | Missing array range in data copy to the GPU                                                                                                                        | x | x       | x   |         |
| [PWD004](Checks/PWD004/) | Out-of-memory-bounds array access                                                                                                                                  | x | x       | x   |         |
| [PWD005](Checks/PWD005/) | Array range copied to or from the GPU does not cover the used range                                                                                                | x | x       | x   |         |
| [PWD006](Checks/PWD006/) | Missing deep copy of non-contiguous data to the GPU                                                                                                                | x | x       | x   |         |
| [PWD007](Checks/PWD007/) | Unprotected multithreading recurrence                                                                                                                              | x | x       | x   |         |
| [PWD008](Checks/PWD008/) | Unprotected multithreading recurrence due to out-of-dimension-bounds array access                                                                                  | x | x       | x   |         |
| [PWD009](Checks/PWD009/) | Incorrect privatization in parallel region                                                                                                                         | x | x       | x   |         |
| [PWD010](Checks/PWD010/) | Incorrect sharing in parallel region                                                                                                                               | x | x       | x   |         |
| [PWD011](Checks/PWD011/) | Missing OpenMP lastprivate clause                                                                                                                                  | x | x       | x   |         |
| [RMK001](Checks/RMK001/) | Loop nesting that might benefit from hybrid parallelization using multithreading and SIMD                                                                          | x | x       | x   |         |
| [RMK002](Checks/RMK002/) | Loop nesting that might benefit from hybrid parallelization using offloading and SIMD                                                                              | x | x       | x   |         |
| [RMK003](Checks/RMK003/) | Potentially privatizable temporary variable                                                                                                                        | x |         | x   |         |
| [RMK007](Checks/RMK007/) | SIMD opportunity within a multithreaded region                                                                                                                     | x | x       | x   |         |
| [RMK008](Checks/RMK008/) | SIMD opportunity within an offloaded region                                                                                                                        | x | x       | x   |         |
| [RMK009](Checks/RMK009/) | Outline loop to increase compiler and tooling code coverage                                                                                                        | x |         | x   |         |
| [RMK010](Checks/RMK010/) | The vectorization cost model states the loop is not a SIMD opportunity due to strided memory accesses in the loop body                                             | x | x       | x   |         |
| [RMK012](Checks/RMK012/) | The vectorization cost model states the loop is not a SIMD opportunity because conditional execution renders vectorization inefficient                             | x | x       | x   |         |
| [RMK013](Checks/RMK013/) | The vectorization cost model states the loop is not a SIMD opportunity because loops with low trip count unknown at compile time do not benefit from vectorization | x | x       | x   |         |
| [RMK014](Checks/RMK014/) | The vectorization cost model states the loop is not a SIMD opportunity due to unpredictable memory accesses in the loop body                                       | x | x       | x   |         |
| [RMK015](Checks/RMK015/) | Tune compiler optimization flags to increase the speed of the code                                                                                                 | x | x       | x   |         |
| [RMK016](Checks/RMK016/) | Tune compiler optimization flags to avoid potential changes in floating point precision                                                                            | x | x       | x   |         |

**AutoFix**: Denotes tools that support automatic correction of the
corresponding check. Readers are encouraged to report additional tools with
autofix capabilities for these checks. The tools are tagged in the table as
follows:

  - **1**: [Codee](https://www.codee.com)

## Contributing

We welcome and encourage contributions to the Open Catalog! Here's how you can
get involved:

1. **Join the discussion:**

    Got ideas, questions, or suggestions? Head over to our [GitHub
    Discussions](https://github.com/codee-com/open-catalog/discussions). It's
    the perfect place for open-ended conversations and brainstorming!

2. **Report issues:**

    Found inaccuracies, unclear explanations, or other problems? Please open an
    [Issue](https://github.com/codee-com/open-catalog/issues). Detailed reports
    help us quickly improve the quality of the project!

3. **Submit pull requests:**

    Interested in solving any issues? Feel free to fork the repository, make
    your changes, and submit a [Pull
    Request](https://github.com/codee-com/open-catalog/pulls). We'd love to see
    your contributions!
