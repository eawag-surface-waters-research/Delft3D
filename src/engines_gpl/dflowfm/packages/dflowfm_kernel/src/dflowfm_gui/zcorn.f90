 double precision function zcorn(k)                  ! get various values at flow cell corners
 use m_flow
 use m_flowgeom
 implicit none

 common /drawthis/ ndraw(50)
 integer :: ndraw

 integer :: k, nodval

 nodval = ndraw(31)
 if (nodval == 2) then
    zcorn = ucnx(k)
 else if (nodval == 3) then
    zcorn = ucny(k)
 else if (nodval == 4) then
    zcorn = sqrt( ucnx(k)*ucnx(k) + ucny(k)*ucny(k) )
 endif
 end function zcorn
