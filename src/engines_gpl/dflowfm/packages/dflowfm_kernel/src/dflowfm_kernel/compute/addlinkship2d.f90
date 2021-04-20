 subroutine addlinkship2D(L,japerim)  ! but substract, nested Newton
 use m_ship
 use m_flowgeom
 use m_flow
 implicit none
 integer          :: L, japerim

 integer          :: k1, k2, k3, k4
 double precision :: BL1, BL2, b21, wu2, ai, wid1, wid2, hpr1, hpr2, dx1, dx2, ar1, ar2
 k1 = ln  (1,L) ; k2 = ln  (2,L)
 k3 = lncn(1,L) ; k4 = lncn(2,L)
 if (zspc(k3) .ne. 0d0 .or. zspc(k4) .ne. 0d0) then
    if (zspc(k3) < zspc(k4)) then
       BL1 = zspc(k3) ; BL2 = zspc(k4)
    else
       BL1 = zspc(k4) ; BL2 = zspc(k3)
    endif
    wu2  = wu(L)   ; b21 = BL2 - BL1 ; ai  = b21/wu2

    if (japerim == 0) then
       hpr1 = s1m(k1)-BL1
       if (hpr1 > 0) then
          call getlinkareawid2D(L,wu2,b21,ai,hpr1,ar1,wid1)
          dx1      = 0.5d0*dx(L)*acl(L)
          a1m(k1)  = a1m(k1)  + dx1*wid1
          vol1(k1) = vol1(k1) - dx1*ar1
       endif

       hpr2 = s1m(k2)-BL1
       if (hpr2 > 0) then
         call getlinkareawid2D(L,wu2,b21,ai,hpr2,ar2,wid2)
         dx2      = 0.5d0*dx(L)*(1d0-acl(L))
         a1m(k2)  = a1m(k2)  + dx2*wid2
         vol1(k2) = vol1(k2) - dx2*ar2
       endif
    else if (hu(L) > 0) then
       hpr1 = 0.5d0*(s1(k1) + s1(k2)) - BL1
       if (hpr1 > 0) then
          call getlinkareawid2D(L,wu2,b21,ai,hpr1,ar1,wid1)
          au(L) = au(L) - ar1
       endif
    endif
 endif
 end subroutine addlinkship2D
