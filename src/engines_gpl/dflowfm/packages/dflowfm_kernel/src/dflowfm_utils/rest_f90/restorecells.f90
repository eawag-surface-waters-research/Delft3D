subroutine restorecells()
   use network_data
   use m_partitioninfo, only: idomain, idomain0
   use m_flowgeom, only: xz, xz0, yz, yz0, ba, ba0
   use m_alloc
   implicit none
   integer  :: ierr

   integer  :: k, N

!   nump1d2d = size(netcell)
   nump1d2d = nump1d2d0
   nump     = nump0

   if(allocated(netcell)) then
      do k=1,ubound(netcell0,1)
         if ( allocated(netcell(k)%nod) ) deallocate(netcell(k)%nod)
         if ( allocated(netcell(k)%lin) ) deallocate(netcell(k)%lin)
      end do
      deallocate(netcell)
   end if

   allocate(netcell(nump1d2d), stat = ierr)
   do k=1,nump1d2d
      N = netcell0(k)%N
      netcell(k)%N = N

      allocate(netcell(k)%nod(N))
      netcell(k)%nod = netcell0(k)%nod(1:N)

      allocate(netcell(k)%lin(N))
      netcell(k)%lin = netcell0(k)%lin(1:N)
   end do

!   if(allocated(netcell))  deallocate(netcell)
!   allocate(netcell(nump1d2d), stat = ierr)
!   netcell(1: nump1d2d) = netcell0(1: nump1d2d)

   call realloc(lne, (/2, numl/), stat=ierr, keepExisting=.false.)
   call realloc(lnn, numl , stat=ierr, keepExisting=.false.)
   lne = lne0
   lnn = lnn0

   call realloc(xz, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yz, nump1d2d, stat=ierr, keepExisting=.false.)
   xz = xz0
   yz = yz0

   call realloc(xzw, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yzw, nump1d2d, stat=ierr, keepExisting=.false.)
   xzw = xzw0
   yzw = yzw0

   call realloc(ba, nump1d2d, stat=ierr, keepExisting=.false.)
   ba = ba0

   if ( allocated(idomain0) ) then
      call realloc(idomain, nump1d2d, stat=ierr, keepExisting=.false.)
      idomain = idomain0
   end if

end subroutine restorecells
