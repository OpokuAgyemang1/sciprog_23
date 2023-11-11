! Find the value e using Talor series expansion

Program T_series
  implicit none
   interface
     function factorial(n)
      integer(kind=4) :: factorial, n
      end function
     end interface
      


    integer(kind=4) :: i, order, ierr
     real(kind=8) :: e
     real(kind=8), allocatable :: terms(:)
     
!  Enter polynomial order
   write(6,*), 'Enter the requested polynomial order'
   read(5,*, iostat=ierr) order
   if(ierr .ne. 0) write(6,*) ' Problem with input'
   
   allocate(terms(order))

    do i=1, order
       terms(i) = 1.0/real(factorial(i), kind=8)
   write(6, '(a,i2,a,f20.16)') 'e term for order',i, 'is', terms(i)
     
   end do
  
  e = dble(1.0)
   write(6,*) ' e is estimated as', e+sum(terms), 'Difference', &
                                             e+sum(terms)-dexp(e)
 
 deallocate(terms)

   stop

End Program T_series

 integer(kind=4) function factorial(n)
   implicit none
   integer(kind=4), intent(in):: n
   integer(kind=4) :: i, x

   x = 1
   do i=1, n
    x= x*i
   end do
   factorial = x
   return
end function factorial
