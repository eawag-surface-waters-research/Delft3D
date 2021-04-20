!> administer a cell
!>    note: cell circumcenters are not updated (would require up-to-date lnn, lne)
subroutine makecell(N, nodlist, linlist, ic, ierror)

   use m_netw
   use m_alloc
   use network_data, only : xzw, yzw
   use m_flowgeom, only: ndx, xz, yz, ba
   use gridoperations

   implicit none

   integer,               intent(in)  :: N       !< number of nodes and links in cell
   integer, dimension(N), intent(in)  :: nodlist !< nodelist
   integer, dimension(N), intent(in)  :: linlist !< linklist
   integer,               intent(out) :: ic      !< cell number
   integer,               intent(out) :: ierror  !< error (1) or not (0)

   integer                            :: numc

   integer                            :: ierr

   real , parameter                   :: growfac = 1.2

   ierror = 1

   call increasenetcells(NUMP+1, growfac, .true.)
   ic = NUMP+1
   call realloc(netcell(ic)%NOD, N, stat=ierr, keepExisting=.false.)
   call realloc(netcell(ic)%LIN, N, stat=ierr, keepExisting=.false.)

   if ( ierr.ne.0 ) then
      return
   end if

   netcell(ic)%N   = N
   netcell(ic)%nod = nodlist
   netcell(ic)%lin = linlist

!  cell circumcenters etcetera
!  the following is taken from update_cell_circumcenters(), however:
!     keepExisting=.true. instead of keepExisting = .false.
!     the array sizes are increased with an additional growfactor
   if (nump+1 > size(xz)) then
      numc = ceiling(growfac*dble(max(ndx+1,nump+1)))
      call realloc(xz, numc, stat=ierr, keepExisting=.true.)
      call aerr('xz(numc)',IERR, numc)
      call realloc(yz, numc, stat=ierr, keepExisting=.true.)
      call aerr('yz(numc)',IERR, numc)
      call realloc(xzw, numc, stat=ierr, keepExisting=.true.)
      call aerr('xzw(numc)',IERR, numc)
      call realloc(yzw, numc, stat=ierr, keepExisting=.true.)
      call aerr('yzw(numc)',IERR, numc)
      call realloc(ba, numc, stat=ierr, keepExisting=.true.)
      call aerr('ba(numc)',IERR, numc)
   endif

!  update nump
   nump = nump + 1

   ierror = 0

!  error handling
1234 continue

   return
end subroutine makecell
