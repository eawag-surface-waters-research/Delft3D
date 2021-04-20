 double precision function dcentral(d1,d2)                     ! twee maal vergroot vanwege acl
 implicit none
 double precision d1, d2, dcminmod
 if (d1*d2 > 0d0) then
   dcentral = dcminmod( (d1+d2)*0.5d0 , dcminmod( 2d0*d1, 2d0*d2) )
 else
   dcentral = 0d0
 endif
 return
 end function dcentral
