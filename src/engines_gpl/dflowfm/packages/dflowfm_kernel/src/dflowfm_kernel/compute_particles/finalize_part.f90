subroutine finalize_part()
   use m_particles
   implicit none

   call dealloc_partmesh()
   call dealloc_partfluxes()
   call dealloc_partrecons()
   call dealloc_particles()
   call dealloc_auxfluxes()
   call dealloc_partparallel()
   japart = 0

   return
end subroutine finalize_part
