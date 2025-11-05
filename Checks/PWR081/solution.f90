! PWR081: Uninitialized output arguments can lead to undefined behavior

module StatsMod
  use iso_fortran_env, only: real32
  implicit none
  private

  public :: computeStats, statsType

  type :: statsType
    real(kind=real32) :: mean
    real(kind=real32) :: stddev
  end type statsType

contains

  pure subroutine computeStats(data, result)
    real(kind=real32), intent(in) :: data(:)
    type(statsType), intent(out) :: result

    integer :: i, len

    result%mean = 0.0
    result%stddev = 0.0

    ! We can't compute stddev from a single data point
    len = size(data)
    if (len <= 1) then
      return
    end if

    ! Also abort if there are invalid data points
    do i = 1, len
      if (data(i) < -900.0) then
        return
      end if
    end do

    result%mean = sum(data) / len
    result%stddev = sqrt(sum((data - result%mean)**2) / (len - 1))
  end subroutine computeStats
end module StatsMod

program main
  use iso_fortran_env, only: real32
  use StatsMod, only: computeStats, statsType
  implicit none

  real(kind=real32), dimension(5) :: sensorData = [1.0, -999.0, 3.0, 5.0, 7.0]
  type(statsType) :: stats

  call computeStats(sensorData, stats)
  print '(A, F4.2, A, F4.2)', "Mean = ", stats%mean, " Stddev = ", stats%stddev
end program main
