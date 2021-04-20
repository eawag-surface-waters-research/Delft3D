   subroutine corioliskelvin(tim)
    use m_netw
    use m_flowgeom
    use m_flow
    use m_sferic
    use unstruc_display
    implicit none

    integer          :: k, L, k1, k2
    double precision :: tim,s1k, xx, yy, samp, ux, uy, dif, alf, cs, sn, aer, dep, r0, x0, y0, Rossby, rr, sgh

    if (tim == 0d0) then
       call inisferic()
    endif

    dep    = sini-zkuni
    sgh    = sqrt(ag/dep)
    Rossby = sqrt(ag*dep) / fcorio
    r0     = 0.5d0*(xkmax-xkmin)
    x0     = 0.5d0*(xkmax+xkmin)
    y0     = 0.5d0*(ykmax+ykmin)
    samp   = 0.05d0

    call statisticsnewstep()

    do k   = 1,ndx
       yy  = yz(k)  - y0
       xx  = xz(k)  - x0
       rr  = dsqrt(xx*xx + yy*yy)
       cs  = xx/rr
       sn  = yy/rr

       aer = samp*exp((rr-r0)/Rossby)
       s1k = aer*cs

       if (tim == 0) then
          s1(k)  = max( bl(k), s1k) ; s0(k) = s1(k)
          ucx(k)  = -s1k*sgh*sn
          ucy(k)  =  s1k*sgh*cs
       endif

       dif = abs(s1(k) - s1k)
       call statisticsonemorepoint(dif)
    enddo

    if (tim == 0) then
       do L  = 1,Lnx
          k1 = ln(1,L) ; k2 = ln(2,L)
          u1(L) = 0.5d0*(ucx(k1) + ucx(k2))*csu(L) + 0.5d0*(ucy(k1) + ucy(k2))*snu(L)
          u0(L) = u1(L)
       enddo
    endif

    call statisticsfinalise()
    end subroutine corioliskelvin
