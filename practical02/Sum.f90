
program sum
! Declare variables
   implicit none
   integer (kind=4) :: i
   real (kind=4) :: sum1, sum2, diff, x
   

! First sum
   sum1 = 0.0
   do i=1,1000
   x = i
   sum1 = sum1 + 1.0/x
   end do


! Second sum 
   sum2 = 0.0
   do i=1000,1,-1
    x = i
      sum2 = sum2 + 1.0/x
   end do

   write(6,*) ' Sum1=',sum1
   write(6,*) ' Sum2=',sum2

! Find the difference
   diff = sum1 - sum2

   write(6,*) ' Difference between the two is ',diff

end program sum
