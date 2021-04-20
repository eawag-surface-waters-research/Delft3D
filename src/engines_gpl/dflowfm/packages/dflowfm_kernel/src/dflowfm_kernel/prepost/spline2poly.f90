!> copy the spline to a polyline
subroutine spline2poly()
   use m_splines
   use m_spline2curvi
   use m_gridsettings
   use m_polygon
   use m_missing

   implicit none

   double precision, allocatable, dimension(:) :: sc  !  spline-coordinates of grid points, not used

   integer                                     :: ispline, num, numpoints, kmax, mfacmax

   double precision                            :: hmax

   call savepol()
   call delpol()

   mfacmax = mfac

   allocate(sc(mfacmax+1))

   numpoints = 0
   do ispline=1,mcs
!     determine the number of control points in the spline
      call nump(ispline,num)

      if ( splineprops(ispline)%id .eq. 0 ) then    ! center splines only
         if ( numpoints.gt.0 ) then   ! add to existing polygon
!           add DMISS
!            numpoints = numpoints+mfac_loc(ispline)+1+1
            call increasepol(numpoints+mfacmax+2, 0 )
            npl = npl+1
            xpl(npl) = DMISS
            ypl(npl) = DMISS
         else  ! no existing polygon
!            numpoints = numpoints+mfac_loc(ispline)+1
            call increasepol(numpoints+mfacmax+1, 0)
         end if

         mfac = splineprops(ispline)%mfac
         hmax = splineprops(ispline)%hmax
         call make_gridline(num, xsp(ispline,1:num), ysp(ispline,1:num), dwidth, mfacmax, mfac, hmax, xpl(npl+1:numpoints), ypl(npl+1:numpoints), sc, jacurv)
         numpoints = numpoints+mfac+1
         npl = numpoints
      end if
   end do

   deallocate(sc)

!  restore
   mfac = mfacmax

   return
end subroutine spline2poly
