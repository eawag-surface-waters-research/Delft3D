  subroutine tegenovernodesandlink(np,LL,k1a,k2a,La)
  use m_netw
  implicit none
  integer :: np,LL,k1a,k2a,La

  integer :: lk
  integer :: n
  integer :: na

  do n  = 1,netcell(np)%n
     Lk = netcell(np)%lin(n)
     if (Lk == LL) then
        exit
     endif
  enddo

  if (n == 1) then
     na = 3
  else if (n == 2) then
     na = 4
  else if (n == 3) then
     na = 1
  else if (n == 4) then
     na = 2
  endif

  La  = netcell(np)%lin(na)
  k1a = kn(1, La)
  k2a = kn(2, La)

  end subroutine tegenovernodesandlink
