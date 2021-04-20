 double precision function skoren(sl1,sl2)
 implicit none
 double precision :: sl1, sl2
 double precision :: r
 r = sl1/sl2
 skoren = max(0d0,min(r+r,min((1d0+r+r)/3d0,2d0)))
 return
 end function skoren
