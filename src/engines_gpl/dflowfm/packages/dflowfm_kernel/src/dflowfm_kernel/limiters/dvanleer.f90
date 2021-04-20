 double precision function dvanleer(d1,d2)                     ! twee maal vergroot vanwege acl
 implicit none
 double precision d1, d2
 if (d1*d2 > 0d0) then
   dvanleer = 2d0*d2/(d1+d2)
 else
   dvanleer = 0d0
 endif
 return
 end function dvanleer
