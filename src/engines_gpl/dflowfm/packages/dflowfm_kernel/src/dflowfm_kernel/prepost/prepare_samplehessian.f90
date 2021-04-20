!> prepare the sample Hessians
subroutine prepare_sampleHessian(ierror)
   use m_samples
   use m_samples_refine

   implicit none

   integer, intent(out) :: ierror   !< error (1) or not (0)
   integer              :: jacancelled

   ierror = 1

   if ( iHesstat.ne.iHesstat_OK ) then
!     (re)allocate
      call allocate_sampleHessian()

!     copy and possibly smooth sample data to zss(1,:,:)
      call smooth_samples(MXSAM, MYSAM, NS, NDIM, Nsamplesmooth, zs, zss)
      Nsamplesmooth_last = Nsamplesmooth

!     compute sample Hessians
      call comp_sampleHessian(ierror)
      if ( ierror.ne.0 ) goto 1234
   end if

   iHesstat = iHesstat_OK

   ierror = 0
1234 continue

   return
end subroutine prepare_sampleHessian
