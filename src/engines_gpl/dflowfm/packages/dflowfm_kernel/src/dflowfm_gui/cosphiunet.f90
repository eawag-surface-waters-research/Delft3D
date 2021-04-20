 double precision function cosphiunet(L)                ! get link cos on net link

 use m_flowgeom
 use m_netw
 use geometry_module, only: dcosphi
 use m_sferic, only: jsferic, jasfer3D
 use m_missing, only : dxymis

 implicit none


 integer :: L                                        ! for net link L,

 ! locals
 integer          :: k1, k2, k3, k4
 double precision :: rn, rt, rnl, rtl

 ! Check: no findcells done yet. Report 'all bad'.
 if (nump <= 0) then
    cosphiunet = 1
    return
 end if

 ! Check: 1D or closed boundary link: report 'good'.
 if (lnn(L) < 2) then
    cosphiunet = 0
    return
 elseif (lne(1,L) <= 0 .or. lne(2,L) <= 0) then
    cosphiunet = 0
    return
 elseif (kn(1,L) <= 0 .or. kn(2,L) <= 0) then
    cosphiunet = 0
    return
 end if

 k1 = lne(1,L)
 k2 = lne(2,L)
 k3 = kn(1,L)
 k4 = kn(2,L)
 cosphiunet = dcosphi(xz(k1), yz(k1), xz(k2), yz(k2), xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dxymis)

 end function cosphiunet
