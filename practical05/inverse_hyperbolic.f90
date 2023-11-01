MODULE CONSTS
  INTEGER (KIND=4), PARAMETER :: LENGTH = 1000
END MODULE CONSTS

MODULE HYPOB
 
 CONTAINS
 ! Calculate the inverse hyperbolic tangent of real 'x'
 ! with a precision error of less than 'delta'

      function artanh(x, delta)
      implicit none
      real (kind=8) :: artanh, x, delta
      integer (kind=8) :: n
      real (kind=8) :: arctan,elem, val
      arctan = x; n = 1;

      elem = x
      do while (abs(elem) .ge. delta)
         val = 2*n + 1
         elem = x**val/val
         arctan = arctan + elem
         n = n + 1
         end do
         arctan = arctan
         return
        end function artanh

 ! Calculate the inverse hyperbolic tangent of real 'x'
      function artanh2(x)
       implicit none
       real (kind=8) :: artanh2, x
       artanh2 = (log(1.0+x) - log(1.0-x))/2
       return
      end function artanh2
END MODULE

PROGRAM INVHYPO
    use CONSTS
    use HYPOB
    implicit none
    real (kind=8) :: x, delta, tan1(length), tan2(length)
    integer (kind=4) :: j=1

 ! Reading the precition to be applied to the numerical method
   write(6,*) 'Enter the method''s precision in (0.1):'
   read(5,*) delta
   x= -0.9
   do while(x .le. 0.9 .AND. j .le. length)
      tan1(j) = artanh(x, delta)
      tan2(j) = artanh2(x)
      write(6, fmt='(a, f20.10,a,f20.10)') 'Difference at ', x, 'is', &
      abs(tan1(j)- tan2(j))
      j=j+1
      x=x+0.1
   end do

   stop

END PROGRAM INVHYPO

