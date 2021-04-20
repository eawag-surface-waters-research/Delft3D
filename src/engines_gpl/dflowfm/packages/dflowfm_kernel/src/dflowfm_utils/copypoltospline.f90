!> copy polygon to spline
  SUBROUTINE COPYPOLTOSPLINE()
  use m_polygon
  use m_splines
  use m_missing
  use geometry_module, only: get_startend
  implicit none

  integer :: k
  integer :: jstart, jend, jpoint

  jpoint = 1
  do while ( jpoint.le.NPL )
     call get_startend(NPL-jpoint+1,xpl(jpoint:NPL),ypl(jpoint:NPL),jstart,jend, dmiss)

     jstart = jstart+jpoint-1
     jend   = jend+jpoint-1

     if ( jend-jstart+1.gt.1 ) then
        call addSplinePoints(mcs+1, xpl(jstart:jend), ypl(jstart:jend))
     end if

     jpoint = jend+1
  end do
  CALL delpol()

  RETURN
  END SUBROUTINE COPYPOLTOSPLINE
