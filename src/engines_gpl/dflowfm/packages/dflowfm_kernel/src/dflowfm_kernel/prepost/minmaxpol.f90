   SUBROUTINE MINMAXPOL(XMIN, YMIN, XMAX, YMAX)
   USE M_POLYGON
   USE M_MISSING
   implicit none
   double precision :: XMIN, YMIN, XMAX, YMAX

   integer :: k
   XMAX = -1E30; XMIN = -XMAX
   YMAX = -1E30; YMIN = -YMAX
   DO K = 1,NPL
      IF (XPL(K) .NE. XYMIS) THEN
         XMAX = MAX(XPL(K),XMAX)
         YMAX = MAX(YPL(K),YMAX)
         XMIN = MIN(XPL(K),XMIN)
         YMIN = MIN(YPL(K),YMIN)
      ENDIF
   ENDDO
   END SUBROUTINE MINMAXPOL
