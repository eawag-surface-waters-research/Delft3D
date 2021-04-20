 double precision function coshsafe(a)
 implicit none
 double precision :: a
 if (a < 9d0) then
    coshsafe = cosh(a)
 else
    coshsafe = 0.5d0*exp(a)
 endif
 end function
