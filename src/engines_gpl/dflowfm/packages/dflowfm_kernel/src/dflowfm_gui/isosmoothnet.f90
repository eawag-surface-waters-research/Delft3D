 subroutine isosmoothnet(k) ! smooth isolines in net cells
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer :: k

 integer          :: nn4, n, inode
 double precision :: xx(10), yy(10), zz(10)

 nn4 = size(netcell(k)%nod)
 do n = 1, nn4
    inode = netcell(k)%nod(n)
    xx(n) = xk(inode)
    yy(n) = yk(inode)
    zz(n) = rnod(inode)
 enddo
 call isofil(xx, yy, zz, nn4, 0)
 end subroutine isosmoothnet
