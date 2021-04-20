!> disable ghostcells that have no internal cells in the other subdomains by setting the wu of their flowlinks to zero
!>   and rely on sethu to disable the flowlinks
!>   an invalid ghostcell is a flownode, say k, that:
!>     -is not a boundary node (k.le.Ndxi), and
!>     -is not in the own subdomain (idomain(k).ne.my_rank), and
!>     -is not a member of ghostlist_sall
subroutine disable_invalid_ghostcells_with_wu()
   use m_partitioninfo
   use m_flowgeom, only: Ndx, Ndxi, nd, wu
   implicit none

   integer, dimension(:), allocatable          :: imask
   integer                                     :: i, k, L
   integer                                     :: ierror

   ierror = 0  ! so far, so good
   if ( jampi.eq.0 ) return   ! nothing to do

   ierror = 1

!  allocate
   allocate(imask(Ndx)) ! safety, could also be Ndxi

!  mark the flownodes in the ghostlist
   imask = 0
   do i=1,nghostlist_sall(ndomains-1)
      k = ighostlist_sall(i)
      imask(k) = 1
   end do

!  check non-boundary flownodes and disable cells that are neither in own subdomain nor in ghostlist by setting wu's of their flowlinks to zero
   do k=1,Ndxi
      if ( imask(k).eq.0 .and. idomain(k).ne.my_rank ) then
         do i=1,nd(k)%lnx
            L = iabs(nd(k)%ln(i))
            wu(L) = 0d0
         end do
      end if
   end do

   ierror = 0
1234 continue

   if ( allocated(imask) ) deallocate(imask)

   return
   end subroutine disable_invalid_ghostcells_with_wu
