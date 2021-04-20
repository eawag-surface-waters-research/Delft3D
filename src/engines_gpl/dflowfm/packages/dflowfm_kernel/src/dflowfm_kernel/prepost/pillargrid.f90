!> create pillar grid in polygon
subroutine pillargrid(ierror)
   use m_grid
   use m_gridsettings
   use m_polygon
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance, get_startend

   implicit none

   integer, intent(out)       :: ierror   ! error (1) or not (0)

   integer                    :: i, j, jstart, jend, num, ipol

   double precision           :: R0, R1, R, x0, y0, x1, y1, alpha, beta

   ierror = 1

   if ( NPL.lt.3 ) goto 1234

!  get the first polygon
   call get_startend(NPL,XPL,YPL,jstart,jend, dmiss)

!  number of points in the polygon
   num = jend-jstart+1
   if ( num.lt.2 ) goto 1234  ! we need at least two points in the polygon

!  set the grid sizes
   mc = num+1
   nc = nfac+1

   call increasegrid(mc,nc)

   xc = DMISS
   yc = DMISS

!  construct the grid
   R0 = pil_rad
   x0 = pil_x
   y0 = pil_y

   do i=1,mc
!     get the coordinates of the point on the polyline
      ipol = jstart+i-1
      if ( ipol.gt.jend ) ipol = ipol-num
      x1 = xpl(ipol)
      y1 = ypl(ipol)

!     make the gridline from the pillar to the polygon point
      do j=1,nc
         R1 = dbdistance(x0, y0, x1, y1, jsferic, jasfer3D, dmiss)
!        determine relative position on the gridline
!        uniform:
         alpha = dble(j-1)/dble(nc-1)
         beta  = (1d0-alpha)*R0/R1 + alpha

         xc(i,j) = x0 + beta*(x1-x0)
         yc(i,j) = y0 + beta*(y1-y0)
      end do   ! do j=1,nc
   end do   ! do i=1,mc

   ierror = 0

!  error handling
1234 continue

   return
end subroutine pillargrid
