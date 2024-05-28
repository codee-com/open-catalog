! PWR069: Use the keyword only to explicitly state what to import from a module

module utils
  implicit none
contains

function cosine_distance_f(x1, y1, x2, y2)
  use iso_c_binding, only : c_double

  implicit none
  real(kind=c_double) :: cosine_distance_f, dot_product, magnitude_x, &
                         magnitude_y
  real(kind=c_double), intent(in) :: x1, y1, x2, y2

  dot_product = x1 * y1 + x2 * y2
  magnitude_x = sqrt(x1**2 + x2**2)
  magnitude_y = sqrt(y1**2 + y2**2)

  cosine_distance_f = 1.0 - dot_product / (magnitude_x * magnitude_y)
end function cosine_distance_f

function euclidean_distance_f(x1, y1, x2, y2)
  use iso_c_binding, only : c_double

  implicit none
  real(kind=c_double) :: euclidean_distance_f
  real(kind=c_double), intent(in) :: x1, y1, x2, y2

  euclidean_distance_f = sqrt((y1 - x1)**2 + (y2 - x2)**2)
end function euclidean_distance_f

end module utils
