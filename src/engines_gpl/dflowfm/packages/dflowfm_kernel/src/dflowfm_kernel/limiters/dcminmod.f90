 double precision function dcminmod(d1,d2)                     ! basic minmod definition
 implicit none
 double precision d1, d2
 if (d1*d2 > 0) then
    if (abs(d1) < abs(d2)) then
       dcminmod = d1
    else
       dcminmod = d2
    endif
 else
    dcminmod = 0d0
 endif
 return
 end function dcminmod
