 subroutine drybed(time)
 implicit none
 double precision :: time, xm, xmx, h0, dxw
 integer, parameter :: mmax = 601 !  3000
 double precision:: s(0:mmax),u(0:mmax),xx(0:mmax)
 double precision :: g, t, cw, xl, xr,sa,ua, x
 integer :: m


 g   = 9.81 ! 10.0
 t   = time
 h0  = 2
 cw  = sqrt(9.81*h0)
 dxw = 100.
 xl  = -cw*t
 xr  = t*2*cw

 xmx = -9999
 do m=2, 600 ! ndx
    xm = m*dxw - 50  ! xz(m)
    x  = xm - 300*dxw
    if (x.gt.xl.and.x.lt.xr) then
       sa=(((2*cw-x/t)/3.0)**2)/g
       ua=2*(cw+x/t)/3.0
       xmx = max (xmx, xm)
    else if (x.lt.xl) then
       sa=h0
       ua=0.0
    else
       sa=0.0
       ua=0.0
    endif
    s(m) = sa ; u(m) = ua; xx(m) = xm
    if (m == 2) then
       call movabs(xm,sa)
    else
       call lnabs(xm,sa)
    endif
 enddo
 call movabs(xmx,0.1d0*h0)
 call  lnabs(xmx,0.2d0*h0)

 call compareanalytic(s,u,xx,mmax)
 end subroutine drybed
