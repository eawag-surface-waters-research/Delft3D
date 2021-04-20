    subroutine thacker2dorg(t, ini, rms)
    use m_flowgeom
    use m_flow
    use m_sferic
    implicit none
    double precision :: t, rms
    integer          :: ini, k, L
    double precision :: xzmin, xzmax, yzmin, yzmax, s1k, x0, y0, r0, xx, yy, r, dep,omeg,psi,samp,st,ct,ux,uy
    CALL DMINMAX(   xz, ndx, xzmin, xzmax, ndx)
    CALL DMINMAX(   yz, ndx, yzmin, yzmax, ndx)
    r0  = 0.5d0* (xzmax - xzmin)
    x0  = 0.5d0* (xzmax + xzmin)
    y0  = 0.5d0* (yzmax + yzmin)

    dep  = 10d0
    omeg = twopi/(12*3600)  ! period = 12 hrs
    r0   = sqrt( 2d0*ag*dep/ ( omeg*( omeg+fcorio) ) )  ! Casulli 2007 (19) mind you, no - sign in front of fcorio


!    r0   = sqrt( 2d0*ag*dep/ ( omeg*omeg ) )
!    omeg = 0.5d0*(-fcorio+ sqrt(fcorio*fcorio + 4d0*(2d0*ag*dep/(r0*r0) )  )  )
!    r0   = sqrt( 2d0*ag*dep/ ( omeg*( omeg+fcorio) ) )  ! to keep constant r0, irrespective of fcorio


    psi  = 0.15d0*r0
    samp = psi*dep/(r0*r0)
    st   = sin(omeg*t)
    ct   = cos(omeg*t)
    rms  = 0d0
    do k = 1,ndx
       xx     = xz(k) - x0 ; yy     = yz(k) - y0
       r     = sqrt(  xx*xx + yy*yy )
       bl(k) = -dep*( 1d0 - (r*r)/(r0*r0) )
       s1k   = max( bl(k), samp*(2d0*xx*ct  - 2d0*yy*st - psi) )
       if (ini == 1) then
          s1(k) = s1k
       else
          rms   = rms  + abs (s1k - s1(k))  ! **2
       endif
    enddo
    !rms = sqrt(rms)/ndx
    rms = rms/ndx


    if (ini == 1) then
       ux = -psi*omeg*st
       uy = -psi*omeg*ct
       do L = 1,lnx
          u1(L) = ux*csu(L) + uy*snu(L)
       enddo

       call setbobs()
    endif
    end subroutine thacker2dorg
