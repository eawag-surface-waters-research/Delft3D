 double precision function rkoren(sl1,sl2)                       ! nog naar kijken
 implicit none
 double precision :: sl1, sl2
 double precision :: r
 if (sl1*sl2.GT.0d0) Then
    r=sl2/sl1
    rkoren=max(0d0,min(r+r,min((1d0+r+r)/3d0,2d0)))
 else
    rkoren=0d0
 endif
 return
 end function rkoren
