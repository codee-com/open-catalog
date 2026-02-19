program solution_with_swapped_arguments
  use mpi
  implicit none
  
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
end program solution_with_swapped_arguments
