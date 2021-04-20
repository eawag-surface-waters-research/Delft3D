      SUBROUTINE HITLIN(P1,P2,X1,Y1,X2,Y2,V,XHIT,YHIT,JA)
      implicit none
      double precision :: dp, dv, dx, dy, frac, p1, p2, v, x1, x2, xhit, y1, y2, yhit
      integer          :: ja
      ! SNIJDT EEN ISOLIJN EEN LIJNTJE ?
      DX   = X2 - X1
      DY   = Y2 - Y1
      DP   = P2 - P1
      DV   = V  - P1
      IF (DP .NE. 0) THEN
         FRAC = DV/DP
      ELSE IF (V .EQ. P2) THEN
         FRAC = 1d0
      ELSE
         FRAC = 0
      ENDIF
      JA = 0
      IF (0d0 .LT. FRAC .AND. FRAC .LE. 1d0) THEN
         JA   = 1
         XHIT = X1 + FRAC*DX
         YHIT = Y1 + FRAC*DY
      ENDIF
      RETURN
      END
