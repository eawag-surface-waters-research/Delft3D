!> disable network nodes/samples outside curvilinear grid
subroutine disable_outside_curvigrid(Nk, Ns, xk, yk, xs, ys, imaskk, imasks)
   use m_grid
   use m_polygon
   use m_missing
   use geometry_module, only: dbpinpol

   implicit none

   integer,                         intent(in)    :: Nk      !< number of network nodes
   integer,                         intent(in)    :: Ns      !< number of samples

   double precision, dimension(Nk), intent(in)    :: xk, yk  !< network node coordinates
   double precision, dimension(Ns), intent(in)    :: xs, ys  !< sample  coordinates
   integer,          dimension(Nk), intent(out)   :: imaskk  !< network nodes inside curvigrid (1) or not (0)
   integer,          dimension(Ns), intent(out)   :: imasks  !< samples       inside curvigrid (1) or not (0)

   integer                                        :: i
   integer                                        :: in

   integer                                        :: ierror

   ierror = 1

   imaskk = 0
   imasks = 0

!  store polygon
   call savepol()

!  delete polygon
   call delpol()

!  copy curvigrid boundaries to polygon
   call copycurvigridboundstopol()

   in = -1

   do i=1,Nk
      call dbpinpol(xk(i), yk(i), in,dmiss, JINS, NPL, xpl, ypl, zpl)
      if ( in.eq.1) then
         imaskk(i) = 1
      end if
   end do

   do i=1,Ns
      call dbpinpol(xs(i), ys(i), in,dmiss, JINS, NPL, xpl, ypl, zpl)
      if ( in.eq.1) then
         imasks(i) = 1
      end if
   end do

   ierror = 0
1234 continue

!  restore polygon
   call restorepol()

   return
end subroutine disable_outside_curvigrid
