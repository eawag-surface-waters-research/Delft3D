 subroutine getAVCOR( n, xz, yz, zz )                  ! average coordinate values
 use m_netw
 implicit none

 double precision :: xz, yz, zz                      !
 integer          :: m, n, nn, k

 xz = 0d0 ; yz = 0d0 ; zz = 0d0
 nn = netcell(n)%n                                      ! zwaartepunt
 do m  = 1,nn
    k  = netcell(n)%NOD(m)
    xz = xz + xk(k)
    yz = yz + yk(k)
    zz = zz + zk(k)
 enddo
 xz = xz / nn
 yz = yz / nn
 zz = zz / nn
 end subroutine getAVCOR
