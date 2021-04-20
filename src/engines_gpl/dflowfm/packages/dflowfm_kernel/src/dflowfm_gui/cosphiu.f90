 double precision function cosphiu(L)                ! get link cos

 use m_flowgeom
 use m_netw
 use geometry_module, only: normalin
 use m_missing, only : dxymis
 use m_sferic, only: jsferic, jasfer3D

 implicit none

 integer :: L                                        ! for link L,

 ! locals
 integer          :: k1, k2, k3, k4
 double precision :: rn, rt, rnl, rtl
 k1 = ln(1,L)
 k2 = ln(2,L)
 k3 = lncn(1,L)
 k4 = lncn(2,L)
 call normalin(xz(k1), yz(k1), xz(k2), yz(k2), rnl, rtl, xu(L), yu(L),jsferic, jasfer3D, dxymis)  ! in pos LL direction
 call normalin(xk(k3), yk(k3), xk(k4), yk(k4), rn , rt,  xu(L), yu(L),jsferic, jasfer3D, dxymis)  ! = normalin (k1,k2)
 cosphiu = rnl*rn + rtl*rt

 end function cosphiu
