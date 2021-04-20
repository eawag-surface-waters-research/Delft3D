!> allocate splineprops array
subroutine allocate_splineprops()
   use m_splines
   use m_spline2curvi
   use m_missing

   implicit none


   integer :: ispline

   allocate(splineprops(mcs))

   do ispline=1,mcs
      allocate(splineprops(ispline)%ics(mcs))
      allocate(splineprops(ispline)%Lorient(mcs))
      allocate(splineprops(ispline)%t(mcs))
      allocate(splineprops(ispline)%cosphi(mcs))
      allocate(splineprops(ispline)%hL(Nsubmax,mcs))
      allocate(splineprops(ispline)%hR(Nsubmax,mcs))
      allocate(splineprops(ispline)%NsubL(mcs))
      allocate(splineprops(ispline)%NsubR(mcs))

!     initialize
      splineprops(ispline)%id         = -999
      splineprops(ispline)%ncs        = 0
      splineprops(ispline)%length     = DMISS
      splineprops(ispline)%hmax       = DMISS
      splineprops(ispline)%ics(:)     = 0
      splineprops(ispline)%Lorient(:) = .true.
      splineprops(ispline)%t(:)       = DMISS
      splineprops(ispline)%cosphi(:)  = DMISS
      splineprops(ispline)%hL(:,:)    = DMISS
      splineprops(ispline)%hR(:,:)    = DMISS
      splineprops(ispline)%NsubL(:)   = 0
      splineprops(ispline)%NsubR(:)   = 0

      splineprops(ispline)%mfac      = 0
      splineprops(ispline)%nfacL(:)  = 0
      splineprops(ispline)%nfacR(:)  = 0
      splineprops(ispline)%iL        = 0
      splineprops(ispline)%iR        = 0
   end do
   return
end subroutine allocate_splineprops
