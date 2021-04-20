 double precision function rvanleer(sl1,sl2)                     ! twee maal vergroot vanwege acl
 implicit none
 double precision :: sl1, sl2
 if (sl1*sl2.GT.1.0d-2) then
   rvanleer=2*sl2/(sl1+sl2)
 else
   rvanleer=0d0
 endif
 return
 end function rvanleer
