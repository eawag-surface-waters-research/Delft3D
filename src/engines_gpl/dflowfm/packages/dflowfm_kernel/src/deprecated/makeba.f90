 subroutine makeba()  ! recompute ba
 use m_flow
 use m_flowgeom
 use m_netw
 implicit none
 integer          :: k,L,m,n,k1,k2
 double precision :: dxw, aa1

 ba = 0d0   ! ; acl = 0.5d0
 Do L = 1,lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    dxw    = 0.5d0*dx(L)*wu(L)
    ba(k1) = ba(k1) + dxw*acl(L)
    ba(k2) = ba(k2) + dxw*(1d0-acl(L))
 enddo

 do n   = 1, mxwalls
    k1  = walls(1,n)
    aa1 = walls(17,n)
    ba(k1) = ba(k1) + aa1
 enddo

 do L = Lnxi, Lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    ba(k1) = ba(k2)
 enddo

 end subroutine makeba
