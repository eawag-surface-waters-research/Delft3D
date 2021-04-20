!> send/receive particles to/at subdomain 0
subroutine reduce_particles()
   use m_particles
   use m_partparallel
   use m_partitioninfo, only: jampi, my_rank, ndomains
   use m_alloc
   implicit none

   integer :: i, ipart
   integer :: numsend, numrecv

   integer :: ierror

!   return

   if ( japart.eq.0 .or. jampi.eq.0 ) return

!  count number of particles to be sent
   numsend = 0
   do ipart=1,Npart
      if ( kpart(ipart).gt.0 ) then
         numsend = numsend+1
      end if
   end do

!  make send list
   jsend(0)=1
   jsend(1:ndomains) = 1+numsend
   call realloc(isend,numsend,keepExisting=.false.,fill=0)
   i=0
   do ipart=1,Npart
      if ( kpart(ipart).gt.0 ) then
         i=i+1
         isend(i) = ipart
      end if
   end do

!  fill send array
   call part2send(jsend,isend)

!  send/receive data
   call sendrecv_particledata(NDIM,jsend,jrecv)

   if ( my_rank.eq.0 ) then
!     add particles (original data stored in worksnd)
      call recv2part(jrecv(ndomains)-1,workrecv)
   end if

   japartsaved = 1

   return
end subroutine reduce_particles
