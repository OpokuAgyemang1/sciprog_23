 function matmult(n, p, q, a, b) result(c)
  integer(kind=4) :: i, j, k
  integer(kind=4), intent(in) :: n, p, q
  real(kind=8), intent(in) :: a(n, p), b(p,q)
  real(kind=8) :: c(n, q)

  ! Perform matrix multiplication
  DO i = 1, n
    DO j=1, q
      DO k=1, p
        c(i, j)=c(i, j)+a(i,k)*b(k,j)
      END DO
    END DO
  END DO
 end function matmult 






















