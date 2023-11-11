PROGRAM matrixmult
   implicit none

   Integer(kind=4), parameter :: n=5, p=3, q=4
   Real(kind=8) :: a(n, p), b(p, q), c(n, q)
   Integer(kind=4) :: i,j,k
   

   !Initialise A, B, C
   DO i = 1, n
    DO j=1, p
       a(i, j)=i+j
    END DO
   END DO 

   DO i = 1, p
    DO j = 1, q
       b(i, j) = i-j
    END DO
   END DO
   c = 0.0

  ! Perform matrix multiplication
  DO i = 1, n
    DO j=1, q
      DO k=1, p
        c(i, j)=c(i, j)+a(i,k)*b(k,j)
      END DO
    END DO
  END DO

 !Print out matrices
 write(6,*) "This is matrix A"
  do i=1,n 
   do j=1,p
     write(6, '(f3.0)', advance='no') a(i,j)
   end do
   write(6,*)
  end do


 write(6,*) "This is matrix B"
  do i=1,p
   do j=1,q
     write(6, '(f3.0)', advance='no') b(i,j)
   end do
   write(6,*)
  end do



!Print out matrices
 write(6,*) "This is matrix C"
  do i=1,n
   do j=1,q
     write(6, '(f4.0)', advance='no') c(i,j)
   end do
   write(6,*)
  end do

END PROGRAM matrixmult






















