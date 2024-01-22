# Benchmarks for the Open Catalog on best practices for performance

## Requirements

- CMake version 3.10 or higher
- A C++ compiler with C++11 support
- A Fortran 90 compiler (optional if you want to run the Fortran benchmarks)
- Google benchmark library

## Instructions to run the benchmarks

Use CMake to configure and build as a normal project. Execute the `run` target
to run all the available benchmarks, or run-<CHECK ID> to run a specific one.

Or simply run the following command to do it automatically:
```
./run-benchmarks.sh
```

## CMake options

- **`OCB_ENABLE_C`**:BOOL

  Enable the available benchmarks for the C family language (defaults to ON).

- **`OCB_ENABLE_Fortran`**:BOOL

  Enable the available benchmarks for the Fortran language (default to ON if
  there is a recognized Fortran compiler in the system).
