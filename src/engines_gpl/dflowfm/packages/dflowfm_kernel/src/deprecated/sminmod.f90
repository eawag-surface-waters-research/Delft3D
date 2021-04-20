 double precision function sminmod(sl1,sl2)
 implicit none
 double precision :: sl1, sl2
 double precision :: r
 r = sl1/sl2
 sminmod = max(0d0,min(1d0,r))
 return
 end function sminmod
