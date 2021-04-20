 double precision function tminmod(sl1,sl2)                      ! basic minmod definition
 implicit none
 double precision :: sl1, sl2
 if (sl1*sl2 > 0) then
    if (abs(sl1) < abs(sl2)) then
       tminmod = sl1
    else
       tminmod = sl2
    endif
 else
    tminmod = 0d0
 endif
 return
 end function tminmod
