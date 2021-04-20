!> deallocate flux_coeffs
subroutine dealloc_partfluxes()
   use m_partfluxes
   implicit none

   if ( allocated(iflux2link) ) deallocate(iflux2link)
   if ( allocated(jflux2link) ) deallocate(jflux2link)
   if ( allocated(Aflux2link) ) deallocate(Aflux2link)
end subroutine dealloc_partfluxes
