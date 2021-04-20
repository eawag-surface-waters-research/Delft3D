!> MC limiter function for non-equidistant grid
double precision function dlimiter_nonequi(d1,d2,alpha,s)
   implicit none

   double precision, intent(in) :: d1, d2   !< left and right slopes
   double precision, intent(in) :: alpha    !< interface distance
   double precision, intent(in) :: s        !< mesh width ratio DX2/DX1

   double precision             :: r
   double precision, parameter  :: dtol=1d-16

   double precision             :: TWO1, TWO2

   dlimiter_nonequi = 0d0
   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2

   TWO2 = 1d0/max(alpha,dtol)
   TWO1 = TWO2/max(s,dtol)

!  Monotinized Central
   dlimiter_nonequi = max(0d0, min(TWO1*r,TWO2,0.5d0*(1d0+r)) )

end function dlimiter_nonequi
