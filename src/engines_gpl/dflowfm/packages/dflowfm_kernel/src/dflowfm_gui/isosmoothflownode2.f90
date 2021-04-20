subroutine isosmoothflownode2(k) ! smooth isolines in flow cells use depmax2
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer :: k

 integer          :: nn4, n
 double precision :: zz(10)

 nn4 = size(nd(k)%nod)
 do n = 1, nn4
    zz(n) = rnod( nd(k)%nod(n) )
 enddo
 nn4 = min(nn4, size(nd(k)%x) )
 call isofilb(nd(k)%x, nd(k)%y, zz, nn4, 0)
 end subroutine isosmoothflownode2
