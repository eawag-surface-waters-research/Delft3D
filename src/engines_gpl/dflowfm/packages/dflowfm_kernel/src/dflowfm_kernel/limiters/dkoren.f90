 double precision function dkoren(d1,d2)                       ! nog naar kijken
 implicit none
 double precision d1, d2, r
 if (d1*d2 > 0d0) Then
    r=d2/d1
    dkoren=max(0d0,min(r+r,min((1d0+r+r)/3d0,2d0)))
 else
    dkoren=0d0
 endif
 return
 end function dkoren
