 double precision function dminmod(d1,d2)                      ! twee maal vergroot vanwege acl
 implicit none
 double precision d1, d2
 if (d1*d2 > 0d0) then
   dminmod =min(1d0,d2/d1)
 else
   dminmod =0d0
 endif
 return
 end function dminmod
