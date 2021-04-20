 double precision function svanleer(sl1,sl2)
 implicit none
 double precision :: sl1, sl2
 double precision :: r
 r = sl1/sl2
 svanleer = (r + abs(r))/(1d0 + r)
 return
 end function svanleer
