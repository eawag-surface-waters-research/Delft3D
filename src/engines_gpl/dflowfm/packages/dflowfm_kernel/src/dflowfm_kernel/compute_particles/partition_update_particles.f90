! send/receive paticles from other subdomains
subroutine partition_update_particles()
   use m_particles
   use m_partmesh
   use m_partitioninfo
   use m_partparallel
   use m_alloc
   implicit none

   integer                                       :: i, icell, idmn, j, k
   integer                                       :: numsend, numrecv, numnew
   integer                                       :: N, Nadd, Nsize
   integer                                       :: ipoint
   integer                                       :: Nreplace

   integer                                       :: ierror

   if ( jampi.eq.0 ) return

!  make sendlist
   call part_makesendlist(Npart,kpart)

   numsend = jsend(ndomains)-1

!  copy particles to send array
   call part2send(jsend,isend)

!  send/recv data
   call sendrecv_particledata(NDIM,jsend,jrecv)

!  deactive sent particles
   do j=1,jsend(ndomains)-1
      i = isend(j)
      kpart(i) = 0
   end do

!  add received particles
   call recv2part(jrecv(ndomains)-1,workrecv)

   return
end subroutine partition_update_particles
