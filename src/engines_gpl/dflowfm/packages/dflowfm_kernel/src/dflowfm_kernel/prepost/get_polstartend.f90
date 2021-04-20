!> find the start and end index of a polygon
subroutine get_polstartend(NPL, XPL, YPL, ipol, jstart, jend)
!   use m_polygon
   use m_missing, only: dmiss
   use geometry_module, only: get_startend

   implicit none

   integer,                          intent(in)  :: NPL            !< polygon size
   double precision, dimension(NPL), intent(in)  :: XPL            !< polygon x-coordinates
   double precision, dimension(NPL), intent(in)  :: YPL            !< polygon y-coordinates

   integer,                          intent(in)  :: ipol           !< index of a polygon point
   integer,                          intent(out) :: jstart, jend   !< start and end indices of polygon

   integer                                       :: jpoint

   jpoint = 1
   jstart = 1
   jend   = 0
   do while ( ( ipol.lt.jstart .or. ipol.gt.jend ) .and. jpoint.le.NPL)
      call get_startend(NPL-jpoint+1, xpl(jpoint:NPL), ypl(jpoint:NPL), jstart, jend, dmiss)
      jstart = jstart + jpoint-1
      jend   = jend   + jpoint-1
      jpoint = jend+2
   end do

   return
end subroutine get_polstartend
