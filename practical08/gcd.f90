!Iteration and recursive versions of Euclid's algorithm to calculate the 
!to calculate the gcd of two positive integers.

  module gcdfunctions
  interface
   function iterativeGCD(a,b) result(answer)
    implicit none
     integer(kind=4):: temp
     integer(kind=4), intent(inout) :: a, b
     integer(kind=4) :: answer
   end function iterativeGCD

   recursive function recursiveGCD(a,b) result(answer)
    implicit none
    integer(kind=4), intent(in)::a,b
    integer(kind=4):: temp, tempa, tempb, answer

    end function recursiveGCD
   end interface

  end module gcdfunctions

 program gcd

   use gcdfunctions
    integer(kind=4):: a,b, error


   a = 5
   b = 25

   !call fnctions
   write(6, '(a,i2,a,i2,a,i2)'), 'Iterative GCD(',a,',',b,') = ', &
  iterativeGCD(a,b)
   write(6, '(a,i2,a,i2,a,i2)'), 'Iterative GCD(',a,',',b,') = ', &
   recursiveGCD(a,b)
   
   end program gcd


  function iterativeGCD(a,b) result(answer)

  implicit none
  integer(kind=4), intent(in):: a, b
  integer(kind=4):: temp, tempa, tempb, answer

  tempa=a
  tempb=b

  do while(tempb .ne. 0)
     temp=tempb
     tempb=mod(tempa,tempb)
     tempa=temp
 end do


 answer=tempa

 return


 end function iterativeGCD


 recursive function recursiveGCD(a,b) result(answer)
  implicit none
  integer(kind=4), intent(in):: a,b
  integer(kind=4):: answer

  if(b .eq. 0) then
   answer=a

  else

  answer=recursiveGCD(b, mod(a,b))
  end if

  return

  end function recursiveGCD
