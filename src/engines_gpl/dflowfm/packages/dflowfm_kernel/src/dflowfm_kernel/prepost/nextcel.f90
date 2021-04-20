            !   call nextcel(np,La,npb,k1b,k2b,Lb)
  subroutine nextcel(np,LL,npa,k1a,k2a,La) ! give face, link and nodes, opposite to plakrand LL of cel np
  use m_netw
  implicit none
  integer :: LL,np,La,npa,k1a,k2a

  La = 0 ; npa = 0 ; k1a = 0 ; k2a = 0

  if (np == 0) return

  if (lne(1,LL) == np) then   ! find cell behind current np, eindplaat
      npa = lne(2,LL)
  else if (lne(2,LL) == np) then
      npa = lne(1,LL)
  endif

  if (npa == 0) return

  call tegenovernodesandlink(npa,LL,k1a,k2a,La)

  end subroutine nextcel
