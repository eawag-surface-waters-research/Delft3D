 double precision function depumin(k)                ! min hu of node k
 use m_flow
 use m_flowgeom
 implicit none

 integer :: k

 ! locals
 integer :: L, LL                                    ! for link L,

 depumin = 1e9
 do L       = 1, nd(k)%lnx
    LL      = iabs(nd(k)%ln(L))
    depumin = min( depumin,hu(L) )
 enddo
 end function depumin
