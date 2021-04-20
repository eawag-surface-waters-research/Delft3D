subroutine savecells() !! save netcell, lne, lnn, idomain
   use network_data
   use m_partitioninfo, only: idomain, idomain0
   use m_flowgeom, only: xz, xz0, yz, yz0, ba, ba0
   use m_alloc
   implicit none

   integer    :: ierr
   integer    :: k, N

   nump0 = nump
   nump1d2d0 = nump1d2d

   if(allocated(netcell0)) then
      do k=1,ubound(netcell0,1)
         if ( allocated(netcell0(k)%nod) ) deallocate(netcell0(k)%nod)
         if ( allocated(netcell0(k)%lin) ) deallocate(netcell0(k)%lin)
      end do
      deallocate(netcell0)
   end if

   allocate(netcell0(nump1d2d), stat = ierr)
   do k=1,nump1d2d
      N = netcell(k)%N
      netcell0(k)%N = N

      allocate(netcell0(k)%nod(N))
      netcell0(k)%nod = netcell(k)%nod(1:N)

      allocate(netcell0(k)%lin(N))
      netcell0(k)%lin = netcell(k)%lin(1:N)
   end do

!   netcell0(1: nump1d2d) = netcell(1: nump1d2d)

   call realloc(lne0, (/2, numl/), stat=ierr, keepExisting=.false.)
   call realloc(lnn0, numl, stat=ierr, keepExisting=.false.)
   lne0 = lne
   lnn0 = lnn

   call realloc(xz0, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yz0, nump1d2d, stat=ierr, keepExisting=.false.)
   xz0 = xz
   yz0 = yz

   call realloc(xzw0, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yzw0, nump1d2d, stat=ierr, keepExisting=.false.)
   xzw0 = xzw
   yzw0 = yzw

   call realloc(ba0, nump1d2d, stat=ierr, keepExisting=.false.)
   ba0 = ba

   if ( allocated(idomain) ) then
      call realloc(idomain0, nump1d2d, stat=ierr, keepExisting=.false.)
      idomain0 = idomain
   end if

   end subroutine savecells
