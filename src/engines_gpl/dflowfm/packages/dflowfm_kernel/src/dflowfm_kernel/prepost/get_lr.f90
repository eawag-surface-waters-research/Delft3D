!> get left and right neighboring grid layer points
subroutine get_LR(mc, xc, yc, i, iL, iR)

   use m_missing
   use m_spline2curvi
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,                         intent(in)  :: mc     !< grid layer size
   double precision, dimension(mc), intent(in)  :: xc, yc !< grid layer point coordinates
   integer,                         intent(in)  :: i      !< grid layer point

   integer,                         intent(out) :: iL, iR ! left and right neighboring grid layer points

!   double precision, parameter                  :: dtolLR = 1d-1

   integer                                      :: jstart, jend, jacirc_loc

!  check for circular connectivity
   jacirc_loc = jacirc

   jstart = 1
   jend   = mc

!  grid points may be on top of each other: find left neighboring point
   iL = i
   do while ( dbdistance(xc(iL),yc(iL),xc(i),yc(i), jsferic, jasfer3D, dmiss).le.dtolLR )
      if ( jacirc_loc.eq.0 ) then
         if ( iL-1.lt.1 ) exit
      else
         if ( iL-1.lt.jstart ) then
            iL = jend+1
            jacirc_loc = 0  ! only once
         end if
      end if
      if ( xc(iL-1).eq.DMISS .or. yc(iL-1).eq.DMISS ) exit
      iL = iL-1
   end do

!  find right neighboring node
   iR = i
   do while ( dbdistance(xc(iR),yc(iR),xc(i),yc(i), jsferic, jasfer3D, dmiss).le.dtolLR )
      if ( jacirc_loc.eq.0 ) then
         if ( iR+1.gt.mc ) exit
      else
         if ( iR+1.gt.jend ) then
            iR = jstart-1
            jacirc_loc = 0  ! only once
         end if
      end if
      if ( xc(iR+1).eq.DMISS .or. yc(iR+1).eq.DMISS ) exit
      iR = iR+1
   end do

   return
end subroutine get_LR
