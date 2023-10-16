 PROGRAM INTEGRAL

        IMPLICIT NONE
        INTEGER (kind=4) :: N = 12, i
        REAL (kind=4) :: a = 0.0, b_deg = 60.0
        REAL (kind=4) :: pi, b_rad, area, mult_rad

       ! Convert b = pi/3 to radians
        pi = atan(1.0)*4.0
        b_rad = (pi*b_deg)/180.0
        
        !Sum tan(a)+tan(b) where a and b are in radians
        area = tan(a)+tan(b_rad)
        WRITE(6,*) "Initial area (sum at x(0) and x(12))", area

        ! Calculate the area at pts x1, x2, .....x11 and add them up
        ! as in the formular using loops
        !Using do loop
         do i=5, 55, 5
                area = area + 2*tan((pi*i)/180.0)
                write(6, *) 'New area of x(',1/5,') =', area
        end do

        !Multiply area by (pi/3)/2(12) after converting it to radians
        mult_rad=(pi*((b_deg-a)/(2*N)))/180.0;
        area = mult_rad*area
        
        !Approximated result
        WRITE(6,*) "Trapezoidal result is", area
        !REAL result
        !Integral of tan is ln(2)
        WRITE(6,*) "Real result is", log(2.0)


        STOP

END PROGRAM INTEGRAL 
        
