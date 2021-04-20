 double precision function rminmod(sl1,sl2)                      ! twee maal vergroot vanwege acl
 implicit none
 double precision :: sl1, sl2
 if (sl1*sl2.GT.0d0) then
   rminmod=min(1d0,sl2/sl1)
 else
   rminmod=0d0
 endif
 return
 end function rminmod
