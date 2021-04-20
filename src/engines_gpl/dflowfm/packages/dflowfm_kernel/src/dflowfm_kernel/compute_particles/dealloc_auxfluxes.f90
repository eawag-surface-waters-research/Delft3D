!> deallocate auxiliary fluxes
subroutine dealloc_auxfluxes()
   use m_particles
   implicit none

   if ( allocated(sbegin) ) deallocate(sbegin)
   if ( allocated(qpart)  ) deallocate(qpart)

   if ( allocated(qfreesurf) ) deallocate(qfreesurf)

   return
end subroutine dealloc_auxfluxes
