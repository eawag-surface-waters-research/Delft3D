 double precision function sinhsafe(a)
 implicit none
 double precision :: a
 if (a < 9d0) then
    sinhsafe = sinh(a)
 else
    sinhsafe = 0.5d0*exp(a)
 endif
 end function
