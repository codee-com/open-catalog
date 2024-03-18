module gravity_getters
contains
  ! Returns the gravity on planet `planetIndex` in m/s2
  pure function gravity_pure(planetIndex)
    use iso_c_binding, only: c_int, c_double
    
    implicit none
    integer(kind=c_int), intent(in) :: planetIndex
    real(kind=c_double) :: gravity_pure

    select case (planetIndex) 
      case (0) ! Mercury
        gravity_pure = 3.7
      case (1) ! Venus
        gravity_pure = 8.9
      case (2) ! Earth
        gravity_pure = 9.8
      case (3) ! Mars
        gravity_pure = 3.7
      case (4) ! Jupiter
        gravity_pure = 23.1
      case (5) ! Saturn
        gravity_pure = 9.0
      case (6) ! Uranus
        gravity_pure = 8.7
      case (7) ! Neptune
        gravity_pure = 11.0
      case default
        gravity_pure = 0.0
    end select
  end function gravity_pure
  
  function gravity_impure(planetIndex)
    use iso_c_binding, only: c_int, c_double
    
    implicit none
    integer(kind=c_int), intent(in) :: planetIndex
    real(kind=c_double) :: gravity_impure

    select case (planetIndex) 
      case (0) ! Mercury
        gravity_impure = 3.7
      case (1) ! Venus
        gravity_impure = 8.9
      case (2) ! Earth
        gravity_impure = 9.8
      case (3) ! Mars
        gravity_impure = 3.7
      case (4) ! Jupiter
        gravity_impure = 23.1
      case (5) ! Saturn
        gravity_impure = 9.0
      case (6) ! Uranus
        gravity_impure = 8.7
      case (7) ! Neptune
        gravity_impure = 11.0
      case default
        gravity_impure = 0.0
    end select
  end function gravity_impure
end module gravity_getters
