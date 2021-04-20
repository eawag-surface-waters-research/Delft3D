!> restore particles in subdomain 0 after reduce
subroutine restore_particles()
   use m_particles
   use m_partparallel
   use m_partitioninfo
   implicit none

   integer :: i

!   return

   if ( japart.eq.0 .or. jampi.eq.0 ) return

   if ( my_rank.eq.0 .and. japartsaved.eq.1 ) then
!     cleanup large arrays
      call dealloc_particles()
!     restore particles from send array
      call recv2part(jsend(ndomains)-1,worksnd)
   end if

   japartsaved = 0

   return
end subroutine restore_particles
