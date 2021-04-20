 double precision function coshsafei(a) !inverse
 implicit none
 double precision :: a
 if (a < 9d0) then
    coshsafei = 1d0/cosh(a)
 else
    coshsafei = 0d0
 endif
 end function
