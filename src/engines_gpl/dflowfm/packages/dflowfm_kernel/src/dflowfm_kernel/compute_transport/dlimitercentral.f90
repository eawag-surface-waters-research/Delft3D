   double precision function dlimitercentral(dc,d2,limtyp)  ! as dlimiter, now for central gradient instead of slope
   implicit none

   double precision, intent(in) :: dc, d2   !< central and right slopes
   integer         , intent(in) :: limtyp   !< first order upwind (0) or MC (>0)

   double precision             :: r, d1
   double precision, parameter  :: dtol=1d-16

   dlimitercentral = 0d0
   if (limtyp == 0)     return
!   if ( d1*d2.lt.dtol ) return
!
!   r = d1/d2    ! d1/d2
!   r = 2d0*r - 1d0

!  compute left slope (assume uniform mesh)
   d1 = 2d0*dc - d2

   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2

   dlimitercentral = d2 * max(0d0, min(2d0*r,0.5d0*(1d0+r),2d0) ) !  Monotonized Central
end function dlimitercentral
