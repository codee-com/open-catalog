# PWR074: Prefer `use <module>` over `include <source file>`

### Issue

The `include <source file>` directive, while serving a similar purpose to
Fortran's module system, poses several disadvantages. It increases the risk of
errors, hinders code maintainability, and can negatively impact both
compilation speed and executable size.

### Actions

Encapsulate all user code components within importable modules. When
interacting with external libraries, favor using module interfaces. Notable
examples are OpenMP and MPI, which provide both include files and modules:

```diff
-include 'omp_lib.h'
+use omp_lib
```

```diff
-include 'mpif.h'
+use mpi
```

### Relevance

Both `include <source file>` and `use <module>` allow for code to be declared
once and reused across multiple source files. However, they operate at
different abstraction levels. The `include` directive simply inserts the
contents of a file into the destination, line by line. In contrast, `use`
interacts with Fortran's modules, which are high-level objects that offer clear
insights into what is being imported, leading to several advantages:

- **Error prevention:** Modules provide explicit interfaces for all defined
  procedures, enabling compile-time checks to ensure the correctness of calls,
  which is a frequent source of errors in legacy code.

>**Note:**  
>Check the [PWR068 entry](../PWR068) for more details on implicit and explicit
>interfaces!

- **Name collision handling:** While both `include` and `use` can cause name
  collisions when importing their elements, modules mitigate such issues by
  allowing selective imports with `only` and renaming elements elements as
  needed.

>**Note:**  
>Check the [PWR069 entry](../PWR069) for more details on the keyword `only`!

- **Compilation efficiency:** Repeatedly including the same file in different
  project files results in duplicated code, potentially increasing both
  compilation time and executable size. In contrast, modules are compiled once
  and then called as needed.

### Code examples

OpenMP and MPI are common Fortran libraries that offer both include files and
modules for interaction.

Let's start with a traditional MPI code that relies on the include file:

```f90
! example.f90
program example
  implicit none
  include 'mpif.h'

  integer :: buffer, err, rank
  integer, dimension(MPI_STATUS_SIZE) :: status

  call MPI_Init(err)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, err)

  if(rank == 0) then
    call MPI_Recv(buffer, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, err, status)
    write(*,*) "Rank", rank, "received: ", buffer
  else if(rank == 1) then
    buffer = 42
    call MPI_Send(buffer, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, err)
    write(*,*) "Rank", rank, "sent: ", buffer
  endif

  call MPI_Finalize(err)
end program example
```

Since MPI's include files commonly lack explicit procedure interfaces, the code
leads to an unexpected and obscure runtime error:

```txt
$ mpirun --version    
mpirun (Open MPI) 4.1.4
$ mpifort --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ mpifort example.f90 -o example
$ mpirun -np 2 ./example        
 Rank           0 received:            0
 Rank           1 sent:           42

Program received signal SIGSEGV: Segmentation fault - invalid memory reference.

Backtrace for this error:
#0  0x7f1b0fa218c2 in ???
#1  0x7f1b0fa20a55 in ???
#2  0x7f1b0f85b04f in ???
--------------------------------------------------------------------------
Primary job  terminated normally, but 1 process returned
a non-zero exit code. Per user-direction, the job has been aborted.
--------------------------------------------------------------------------
--------------------------------------------------------------------------
mpirun noticed that process rank 0 with PID 0 on node pc exited on signal 11 (Segmentation fault).
--------------------------------------------------------------------------
```

The specific issue is that the `err` and `status` arguments have been
inadvertently swapped in the call to `MPI_Recv()`. By simply replacing the
`include` directive with `use`, the procedures have now explicit interfaces,
allowing to catch the error during compilation:

```f90
! solution_with_swapped_arguments.f90
program solution
  use mpi
  implicit none
  
  ...
```

```txt
$ mpifort solution_with_swapped_arguments.f90 
solution_with_swapped_arguments.f90:12:76:

   12 |     call MPI_Recv(buffer, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, err, status)
      |                                                                            1
Error: There is no specific subroutine for the generic ‘mpi_recv’ at (1)
```

Once `err` and `status` are properly ordered, the code compiles and runs
successfully:

```txt
$ mpifort solution.f90 -o solution
$ mpirun -np 2 ./solution         
 Rank           0 received:           42
 Rank           1 sent:           42
```

### Related resources

- [PWR074 source code examples](../PWR074)

### References

- ["Include files and modules -- Fortran Programming
Language"](https://fortran-lang.org/learn/building_programs/include_files/),
Fortran Community. [last checked July 2024]

- ["Difference between INCLUDE and modules in
Fortran"](https://stackoverflow.com/questions/15662371/difference-between-include-and-modules-in-fortran),
Stack Overflow Community. [last checked July 2024]

- ["Fortran Support Through the mpif.h Include
File"](https://www.mpi-forum.org/docs/mpi-4.1/mpi41-report/node468.htm), MPI
Forum. [last checked July 2024]

- ["OpenMP (The GNU Fortran
  Compiler)"](https://gcc.gnu.org/onlinedocs/gfortran/OpenMP.html), Free
  Software Foundation, Inc. [last checked July 2024]
