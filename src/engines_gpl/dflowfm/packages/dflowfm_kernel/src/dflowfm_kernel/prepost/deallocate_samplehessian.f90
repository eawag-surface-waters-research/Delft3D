!> deallocate sample Hessian data
subroutine deallocate_sampleHessian()
   use m_samples
   use m_samples_refine

   implicit none

   if ( allocated(zss) ) deallocate(zss)

   iHesstat = iHesstat_DIRTY

   return
end subroutine deallocate_sampleHessian
