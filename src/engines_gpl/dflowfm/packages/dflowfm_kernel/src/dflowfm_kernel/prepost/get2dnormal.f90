 subroutine get2Dnormal(n1,xn1,yn1)  ! get x and y components of land normal vector pointing upward.

 use m_flowgeom                      ! = ok for all internal cells
 use m_flow
 use m_netw
 use m_sferic, only: jsferic, jasfer3D
 use m_missing, only : dxymis
 use geometry_module, only: normalin

 implicit none
 integer          :: n1,   k, L, LL, k3, k4
 double precision :: xn1, yn1, a, aa, alf, xt, yt, slope

 xn1 = 0d0 ; yn1 = 0d0; a = 0d0
 do k = 1, size(nd(n1)%ln)
    LL  = nd(n1)%ln(k)
    L   = iabs(LL)
    if (LL < 0) then ! incoming link has positive LL
       alf = acL(L)
    else
       alf = 1d0 - acL(L)
    endif
    aa = alf*wu(L)*dx(L) ; a  = a + aa
    k3 = lncn(1,L)       ; k4 = lncn(2,L)
    call normalin(xk(k3), yk(k3), xk(k4), yk(k4), xt, yt, xu(L), yu(L),jsferic, jasfer3D, dxymis)
    slope = ( zk(k4) - zk(k3) ) / wu(L)
    xn1   = xn1 + aa*xt*slope
    yn1   = yn1 + aa*yt*slope
 enddo
 if (aa > 0d0) then
    xn1 = xn1 / aa
    yn1 = yn1 / aa
 endif
 end subroutine get2Dnormal
