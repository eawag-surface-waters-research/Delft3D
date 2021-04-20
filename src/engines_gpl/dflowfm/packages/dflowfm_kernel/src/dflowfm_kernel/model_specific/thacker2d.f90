    subroutine thacker2d(t, ini)
    use m_netw, only  : xk, yk, zk, numk
    use m_flowgeom
    use m_flow
    use m_sferic
    implicit none
    double precision :: t, rms
    integer          :: ini, k, L, k1, k2
    double precision :: xzmin, xzmax, yzmin, yzmax, s1k, x0, y0, r0, xx, yy, r, dep,omeg,psi,samp,st,ct,ux,uy
    double precision :: h0, zz0, a, a1c, a12, sa12, rr0, ur, ut, cs, sn

    CALL DMINMAX(   xz, ndx, xzmin, xzmax, ndx)
    CALL DMINMAX(   yz, ndx, yzmin, yzmax, ndx)

    r0  = 0.5d0* (xzmax - xzmin)*0.85
    x0  = 0.5d0* (xzmax + xzmin)
    y0  = 0.5d0* (yzmax + yzmin)
    h0  = 10d0
    zz0 =  2d0


    omeg   = twopi/(12*3600)  ! period = 12 hrs
    omeg   = sqrt(8*ag*h0/(r0*r0))

    fcorio = 0d0 ! omeg/2

    a      = ( (h0+zz0)**2 - h0*h0 ) / ( (h0+zz0)**2 + h0*h0 )

    r0     = sqrt( 8d0*ag*h0 / ( omeg*omeg -fcorio*fcorio) )   ! Casulli 2008 (31) mind you, no - sign in front of fcorio

    st     = sin(omeg*t)
    ct     = cos(omeg*t)


    if (ibedlevtyp == 3) then
       do k = 1,numk
          xx    = xk(k) - x0 ; yy     = yk(k) - y0
          r     = sqrt(  xx*xx + yy*yy )
          rr0   = (r*r)/(r0*r0)
          zk(k) = -h0*( 1d0 -  rr0 )
       enddo
       call setbobs()
    endif

    rms    = 0d0
    do k = 1,ndx
       xx    = xz(k) - x0 ; yy     = yz(k) - y0
       r     = sqrt(  xx*xx + yy*yy )
       rr0   = (r*r)/(r0*r0)
       if (ibedlevtyp .ne. 3) then
          bl(k) = -h0*( 1d0 -  rr0 )
       endif

       a1c   = 1d0-a*ct
       a12   = 1d0-a*a
       sa12  = sqrt(a12)

       s1k   = h0*( sa12/a1c - 1d0 - rr0*( a12/(a1c*a1c) -1d0) )
       s1k   = max(bl(k), s1k)
       if (ini == 1) then
          s1(k) = s1k
          ur = omeg*r*a*st/(2d0*a1c)
          ut = ( fcorio*r/(2d0*a1c) )*(sa12 + a*ct -1d0)
          cs = xx/r ; sn = yy/r
          ucx(k) = ur*cs - ut*sn
          ucy(k) = ur*sn + ut*cs
       else
          rms   = rms  + abs (s1k - s1(k)) ! **2
       endif

    enddo
    ! rms = sqrt(rms)/ndx
    rms = rms/ndx


    if (ini == 1) then
       do L = 1,lnx
          k1 = ln(1,L) ; k2 = ln(2,L)
          u1(L) = ( acl(L)*ucx(k1) + (1d0-acl(L))*ucx(k2) ) *csu(L)  &
                + ( acl(L)*ucy(k1) + (1d0-acl(L))*ucy(k2) ) *snu(L)
       enddo

       call setbobs()
    endif

    end subroutine thacker2d
