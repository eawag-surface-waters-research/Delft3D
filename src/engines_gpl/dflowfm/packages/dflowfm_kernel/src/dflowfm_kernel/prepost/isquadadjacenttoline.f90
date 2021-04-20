  subroutine isquadadjacenttoline(L1,n,L2)
  use m_netw
  implicit none
  integer :: L1,n,L2

  integer :: ja
  integer :: l
  integer :: ll
  integer :: k1k, k2k

  double precision :: x1,y1,x2,y2,x3,y3,x4,y4

  L2 = 0
  x1 = xk(kn(1,L1)) ; y1 = yk(kn(1,L1))
  x2 = xk(kn(2,L1)) ; y2 = yk(kn(2,L1))
  do ll = 1,4
     L  = netcell(n)%lin(LL)
     x3 = xk(kn(1,L)) ; y3 = yk(kn(1,L))
     x4 = xk(kn(2,L)) ; y4 = yk(kn(2,L))
     call adjacent(x1,y1,x2,y2,x3,y3,x4,y4,ja,k1k,k2k)
     if (ja == 1) then
        L2 = L
        return
     endif
  enddo
  end subroutine isquadadjacenttoline
