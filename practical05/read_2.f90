program uinput
implicit none
integer (kind=4) :: i, ierr=1
real (kind=8) :: a
write(6,*) 'Enter an int and double'
do while(ierr .ne. 0)
read(5,*,iostat=ierr) i,a
end do
if (ierr .ne. 0) write(6,*) 'Problem with input'
end program uinput
!to compile: gfortran read_2.f90 -o read

