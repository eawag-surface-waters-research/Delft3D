 double precision function rcentral(sl1,sl2)                     ! twee maal vergroot vanwege acl
 implicit none
 double precision :: sl1, sl2
 double precision :: tminmod
 if (sl1*sl2.GT.0d0) then
   rcentral=tminmod( (sl1+sl2)*0.5d0 , tminmod( 2*sl1, 2*sl2) )
 else
   rcentral=0d0
 endif
 return
 end function rcentral
