module fib
  contains
  subroutine fibonacci(a, b)
   integer (kind=4), intent(inout):: a, b
    integer (kind=4) :: next

    ! a = fn-1 b=fn-2 next=fn
    ! on exit: a=fn, b=fn-1

    next =  a + b
    b = a
    a=next
  end subroutine fibonacci

  end module fib

  program fibonacciseries
  use fib
  implicit none
   integer (kind=4):: n, i, f0=0, f1=1

   !Enter a number
   write(6,*), "Enter a positive integer"
   read(5,*), n
  
   !check

   if(n .lt. 1) then
     write(6,*), "The number is not positive"
     stop
    end if

    write(6,*), "The fibonacci sequence is: "
    write(6, '(i0, a, i0, a)', advance = 'NO')
    

    do i = 2, n
    call fibonacci(f1, f0)
    write(6, '(i0, a)', advance='NO'), f1, ','
    if (mod((i+1), 10) .eq. 0) write(6,*)
     end do
 
  end program fibonacciseries
