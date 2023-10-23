! Module with constant values
module consts
  integer (kind=4), parameter :: N = 12
  real (kind=4) :: pi
end module consts

! Module with subroutines
module subroutines
  interface
    subroutine degtorad(deg, rad)
      use consts
      real(kind=4), intent(in) :: deg
      real(kind=4), intent(out) :: rad
    end subroutine degtorad
    subroutine traprule(TanArr, area)
      use consts
      real(kind=4), intent(in) :: TanArr(N+1)
      real(kind=4), intent(out) :: area
    end subroutine traprule
  end interface
end module subroutines

program Trapezoid
  use consts
  use subroutines
  implicit none
  integer (kind=4) :: i
  real (kind=4) :: TanArr(N+1), deg, rad, area

  ! The value of pi 
  pi = atan(1.0) * 4.0

  ! Calculate the values of f(xi) where i=0, ..., 12
  ! and xi are in radians (as array elements)
  do i=1,N+1
    deg = (i-1)*5.0
    call degtorad(deg, rad)
    TanArr(i) = tan(rad)
    write(6,*) 'TanArr[', i, ']= ', TanArr(i), '(f(x) at x= ', i, ')'
  end do

  call traprule(TanArr, area)

  ! Approximated result
  write(6,*) ' Trapezoidal result is ',area
  ! Real result 
  ! Integral of tan is ln(2)
  write(6,*) ' Real result is ',log(2.0)

    stop
end program 

subroutine degtorad(deg, rad)
  use consts
  real(kind=4), intent(in) :: deg
  real(kind=4), intent(out) :: rad
  rad=(pi*deg)/180.0
end subroutine degtorad

subroutine traprule(TanArr, area)
  use consts
  real(kind=4), intent(in) :: TanArr(N+1)
  real(kind=4), intent(out) :: area
  real(kind=4) :: mult_rad
  ! Find the area by trapezodial rule
  area = TanArr(1)+TanArr(N+1);
  ! Calculate the area at pts x1, x2, ..., x11 and add them up
  ! as in the formula using loops 
  do i=2, N, 1
    area=area+2.0*TanArr(i)
  end do
  
  ! Multiply area by (pi/3)/2(12) after converting it to radians 
  call degtorad(((60.0-0.0)/(2.0*N)), mult_rad)
  area=mult_rad*area;
end subroutine traprule
  
