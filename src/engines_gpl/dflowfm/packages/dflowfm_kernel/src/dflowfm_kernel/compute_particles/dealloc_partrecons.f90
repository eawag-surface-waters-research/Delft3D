!> deallocate flux_coeffs
subroutine dealloc_partrecons()
   use m_partrecons
   implicit none

   if ( allocated(qe) ) deallocate(qe)

   if ( allocated(u0x) ) deallocate(u0x)
   if ( allocated(u0y) ) deallocate(u0y)
   if ( allocated(u0z) ) deallocate(u0z)
   if ( allocated(alpha) ) deallocate(alpha)

   if ( allocated(ireconst) ) deallocate(ireconst)
   if ( allocated(jreconst) ) deallocate(jreconst)
   if ( allocated(Areconst) ) deallocate(Areconst)

   return
end subroutine dealloc_partrecons
