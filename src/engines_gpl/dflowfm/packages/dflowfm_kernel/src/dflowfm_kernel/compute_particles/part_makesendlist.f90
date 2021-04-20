!> make CRS-formatted send list
subroutine part_makesendlist(N,icells)
   use m_partmesh
   use m_partitioninfo
   use m_partparallel
   use m_alloc
   implicit none

   integer                          :: N        !< number of cells to be send
   integer, dimension(N)            :: icells   !< cell numbers

!   integer, dimension(0:ndomains)   :: jsend    !< startpointers of send list
!   integer, dimension(N)            :: isend    !< send list

   integer                          :: i, icell, idmn, j, k
   integer                          :: numnew, numsend

!  count number of cells to be sent to other domains
   jsend = 0
   do i=1,N
!     get cell number
      icell = icells(i)

      if ( icell.eq.0 ) cycle

!     get flownode/netcell number
      k = iabs(cell2nod(icell))

!     get domain number
      idmn = idomain(k)

      if ( idmn.eq.my_rank ) cycle

!     update counters
      jsend(idmn+1) = jsend(idmn+1)+1
   end do

!  accumulate
   jsend(0) = 1
   do idmn=0,ndomains-1
      jsend(idmn+1) = jsend(idmn)+jsend(idmn+1)
   end do

   numsend = jsend(ndomains)-1

   if ( numsend.gt.ubound(isend,1) ) then
!     reallocate
      numnew = 1+int(1.2d0*dble(numsend))
      call realloc(isend,numnew,keepExisting=.false.,fill=0)
   end if

!  fill send list
   jpoint = jsend
   do i=1,N
!     get cell number
      icell = icells(i)

      if ( icell.eq.0 ) cycle

      k = iabs(cell2nod(icell))

!     get domain number
      idmn = idomain(k)

      if ( idmn.eq.my_rank ) cycle

      j = jpoint(idmn)

      isend(j) = i

      jpoint(idmn) = jpoint(idmn)+1
   end do

   return
end subroutine part_makesendlist
