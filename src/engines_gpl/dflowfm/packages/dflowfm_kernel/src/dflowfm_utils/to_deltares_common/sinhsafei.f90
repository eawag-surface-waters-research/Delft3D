 double precision function sinhsafei(a) ! inverse
 implicit none
 double precision :: a
 if (a < 9d0) then
    sinhsafei = 1d0/sinh(a)
 else
    sinhsafei = 0d0
 endif
 end function
