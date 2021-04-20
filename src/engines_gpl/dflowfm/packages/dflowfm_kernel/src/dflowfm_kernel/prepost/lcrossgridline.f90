!> check if a line segment crosses the gridline on the center spline
logical function Lcrossgridline(x1,x2,j)

   use m_grid
   use m_missing
   use m_sferic, only: jsferic
   use geometry_module, only: cross

   implicit none

   double precision, dimension(2), intent(in) :: x1, x2  !< coordinates of begin and end point of line segment
   integer,                        intent(in) :: j       !< gridline index

   double precision, dimension(2)             :: x3, x4

   double precision                           :: sL, sm, xcr, ycr, crp

   integer                                    :: i, jacross

   Lcrossgridline = .false.

!   return

   do i=1,mc-1 ! loop over the edges
      x3 = (/ xc(i,j),   yc(i,j) /)
      x4 = (/ xc(i+1,j), yc(i+1,j) /)

      if ( x3(1).eq.DMISS .or. x4(1).eq.DMISS ) cycle

      call cross(x1(1), x1(2), x2(1), x2(2), x3(1), x3(2), x4(1), x4(2), jacross,sL,sm,xcr,ycr,crp, jsferic, dmiss)

      if ( jacross.eq.1 ) then
         Lcrossgridline = .true.
         return
      end if
   end do

   return
end function   ! Lcrosscenterspline
