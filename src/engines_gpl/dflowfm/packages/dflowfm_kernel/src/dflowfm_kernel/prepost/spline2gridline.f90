!> make a gridline on the spline
subroutine spline2gridline(mc, num, xsp, ysp, xsp2, ysp2, xc, yc, sc, h)
!   use m_splines

   implicit none

   integer,                           intent(in)  :: mc       !< number of gridnodes
   integer,                           intent(in)  :: num      !< number of splinenodes
   double precision, dimension(num),  intent(in)  :: xsp, ysp !< splinenode coordinates
   double precision, dimension(num),  intent(inout) :: xsp2, ysp2 ! second order derivatives
   double precision, dimension(mc),   intent(out) :: xc, yc   !< coordinates of grid points
   double precision, dimension(mc),   intent(out) :: sc       !< spline-coordinates of grid points
   double precision,                  intent(in)  :: h        !< for curvature adapted meshing (>0) or disable (<=0)


   double precision, dimension(mc)                :: curv     ! curvature at grid points
   double precision, dimension(mc)                :: ds       ! grid interval in spline coordinates, at grid points
   double precision, dimension(mc)                :: dL       ! grid interval length, at grid points

   double precision, dimension(2)                 :: startstop

   integer                                        :: i, iter, kmax

   if ( mc .lt.2 ) return  ! no curvigrid possible

   startstop = (/0d0,dble(num-1)/)
   call makespl(startstop, xsp, ysp, max(mc,num), num, 2, mc-1, xc, yc, kmax, sc, h)

   if ( kmax.ne.mc ) then
      continue
   end if

   return
end subroutine spline2gridline
