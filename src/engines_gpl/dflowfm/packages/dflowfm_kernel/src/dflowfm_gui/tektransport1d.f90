 subroutine tektransport1D(tim)
 use m_sferic
 use m_statistics
 use m_flowgeom
 use m_flow
 implicit none
 double precision :: tim
 double precision :: cwave, period, omeg, wlen, rk, phi, xx, yy, dif
 integer          :: k

 cwave   = 60d0*sqrt(10d0*1d-4)                ! chezy
 period = 90d0*60d0
 omeg = twopi/period     ! s
 wlen   = cwave*period
 rk     = twopi/wlen
 do k   = 1,600
    xx  = -50d0 + (k-1)*100d0
    phi = rk*xx - omeg*tim
    yy  = 15d0 + 10d0*cos(phi)
    if (k == 1) then
       call movabs(xx,yy)
    else
       call  lnabs(xx,yy)
    endif
 enddo

 if (ndxi < 1) return

 avedif = 0d0
 do k = 1,ndxi
    xx  = xz(k)
    phi = rk*xx - omeg*tim
    yy  = 15d0 + 10d0*cos(phi)
    dif = abs(sa1(k) - yy)
    avedif = avedif + dif
 enddo
 avedif = avedif/ndxi

 end subroutine tektransport1D
