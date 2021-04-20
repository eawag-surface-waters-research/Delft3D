 subroutine which2Dnetlinkwascrossed(NC1,K1,K2,L) ! find the crossed 2D link
 use m_flowgeom
 use m_netw
 use geometry_module, only: cross
 use m_missing, only: dmiss
 use m_sferic, only: jsferic

 implicit none
 integer          :: NC1,K1,K2,LL
 integer          :: nn,kk,kku,jacros,k3,k4,L
 double precision :: SL,SM,XCR,YCR,CRP

 LL = 0
 nn = NETCELL(nc1)%N

 do kk  = 1,nn
    L   = NETCELL(Nc1)%lin(kk)
    K3  = kn(1,L)
    K4  = kn(2,L)

    call CROSS(xk(k1), yk(k1), xk(k2), yk(k2), xk(k3), yk(k3),  xk(k4), yk(k4), JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
    if (jacros == 1) then
       LL = L
       return
    endif
 enddo

 end subroutine which2Dnetlinkwascrossed ! TEMP STORE CROSSED 2d LINK IN LC
