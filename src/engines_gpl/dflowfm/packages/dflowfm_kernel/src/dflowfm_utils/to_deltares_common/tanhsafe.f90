 double precision function tanhsafe(a)
 implicit none
 double precision :: a
 if (a < 9d0) then
    tanhsafe = tanh(a)
 else
    tanhsafe = 1d0
 endif
 end function
