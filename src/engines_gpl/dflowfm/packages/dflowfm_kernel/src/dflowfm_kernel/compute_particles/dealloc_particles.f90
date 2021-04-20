!> deallocate particle data
subroutine dealloc_particles()
   use m_particles
   implicit none

   if ( allocated(xpart)       ) deallocate(xpart)
   if ( allocated(ypart)       ) deallocate(ypart)
   if ( allocated(zpart)       ) deallocate(zpart)
   if ( allocated(dtremaining) ) deallocate(dtremaining)
   if ( allocated(kpart)       ) deallocate(kpart)
   if ( allocated(iglob)       ) deallocate(iglob)

   if ( allocated(numzero)     ) deallocate(numzero)

   Npart = 0

   if ( allocated(trpart)       ) deallocate(trpart)
   if ( allocated(xrpart)       ) deallocate(xrpart)
   if ( allocated(yrpart)       ) deallocate(yrpart)
   if ( allocated(zrpart)       ) deallocate(zrpart)
   Nrpart = 0

   return
end subroutine dealloc_particles
