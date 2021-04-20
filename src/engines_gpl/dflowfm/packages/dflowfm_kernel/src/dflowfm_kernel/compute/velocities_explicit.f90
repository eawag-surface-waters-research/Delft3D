 subroutine velocities_explicit()
 use m_flowgeom
 use m_flow
 use m_flowtimes
 implicit none
 integer :: n, L, LL, k1, k2

 if (itstep == 1) then
    u1 = (u0 - dts*adve)/(1d0 + dts*advi)
    do n  = 1, nbndu  !       boundaries at u points
       L     = kbndu(3,n)
       u1(L) = zbndu(n)
    end do
 endif
 q1   = u1*au

 squ = 0d0 ; sqi = 0d0
 if ( kmx.eq.0 ) then
    do L = 1,lnx
      if (q1(L) > 0) then
          k1 = ln(1,L) ; k2 = ln(2,L)
          squ(k1) = squ(k1) + q1(L)
          sqi(k2) = sqi(k2) + q1(L)
       else if (q1(L) < 0) then
          k1 = ln(1,L) ; k2 = ln(2,L)
          squ(k2) = squ(k2) - q1(L)
          sqi(k1) = sqi(k1) - q1(L)
       endif
    enddo
 else
    do LL = 1,lnx
       do L=Lbot(LL),Ltop(LL)
          if (q1(L) > 0) then
             k1 = ln(1,L) ; k2 = ln(2,L)
             squ(k1) = squ(k1) + q1(L)
             sqi(k2) = sqi(k2) + q1(L)
          else if (q1(L) < 0) then
             k1 = ln(1,L) ; k2 = ln(2,L)
             squ(k2) = squ(k2) - q1(L)
             sqi(k1) = sqi(k1) - q1(L)
          endif
       end do
    enddo
 end if
 end subroutine velocities_explicit
