!> allocate sample Hessian data
subroutine allocate_sampleHessian()
   use m_samples
   use m_samples_refine

   implicit none

   call deallocate_sampleHessian()

   allocate(zss(NDIM,MXSAM,MYSAM))

   iHesstat = iHesstat_DIRTY

   return
end subroutine allocate_sampleHessian
