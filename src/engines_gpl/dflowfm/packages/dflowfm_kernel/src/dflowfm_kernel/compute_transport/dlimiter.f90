!> limiter function
double precision function dlimiter(d1,d2,limtyp)
   implicit none

   double precision, intent(in) :: d1, d2   !< left and right slopes
   integer         , intent(in) :: limtyp   !< first order upwind (0) or MC (>0)

   double precision             :: r
   double precision, parameter  :: dtol=1d-16

   double precision, parameter  :: TWO=2.0d0

   dlimiter = 0d0
   if (limtyp == 0)     return
   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2

!   if ( limtyp.eq.1 ) then
!!     Van Leer
!      dlimiter = dble(min(limtyp,1)) * (r + abs(r) ) / (1 + abs(r) )
!   else
!!     Monotinized Central
      dlimiter = max(0d0, min(TWO*r,TWO,0.5d0*(1d0+r)) )
!   end if

end function dlimiter
