 subroutine volship( )   ! compute ship volume relative to fixed level 0d0
 use m_ship
 use m_flowgeom
 use m_flow
 implicit none

 integer          :: L, k1, k2, k3, k4
 double precision :: BL1, BL2, b21, wu2, ai, wid1, wid2, hpr1, hpr2, dx1, dx2, ar1, ar2, slotsav , dum
 slotsav = slotw2D ; slotw2D = 0d0
 v1ship  = 0d0
 do L = 1,lnx
    k1 = ln  (1,L) ; k2 = ln  (2,L)
    k3 = lncn(1,L) ; k4 = lncn(2,L)
    if (zspc(k3) .ne. 0d0 .or. zspc(k4) .ne. 0d0) then
       if (zspc(k3) < zspc(k4)) then
          BL1 = zspc(k3) ; BL2 = zspc(k4)
       else
          BL1 = zspc(k4) ; BL2 = zspc(k3)
       endif
       wu2  = wu(L)   ; b21 = BL2 - BL1 ; ai  = b21/wu2

       hpr1 = 0d0 - BL1
       if (hpr1 > 0D0) then
          call getlinkareawid2D(L,wu2,b21,ai,hpr1,ar1,wid1)
          dx1         = 0.5d0*dx(L)*acl(L)
          dx2         = 0.5d0*dx(L)*(1d0-acl(L))

          v1ship(k1)  = v1ship(k1) + dx1*ar1
          v1ship(k2)  = v1ship(k2) + dx2*ar1
       endif

    endif
 enddo
 slotw2D = slotsav
 end subroutine volship
