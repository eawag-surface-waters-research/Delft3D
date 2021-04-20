   SUBROUTINE copyPolygonToObservations()
   use m_observations
   USE M_POLYGON
   implicit none

   integer :: n
   DO N = 1,NPL
      call addObservation(XPL(N), YPL(N))
   END DO
   END SUBROUTINE copyPolygonToObservations
