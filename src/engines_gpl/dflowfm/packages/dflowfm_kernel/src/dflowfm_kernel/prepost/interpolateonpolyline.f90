      !> Find a point on a polyline at a certain distance from the start.
      !! The distance is measured along the consecutive polyline segments.
      SUBROUTINE interpolateOnPolyline(X,Y,Z,T,MMAX,XP,YP,ZP,TP,JA)
      implicit none
      DOUBLE PRECISION, intent(in)  :: X(MMAX), Y(MMAX), Z(mmax)  !< The polyline coordinates.
      double precision, intent(in)  :: T(MMAX)           !< Accumulated segment lengths at all points.
      integer,          intent(in)  :: mmax              !< Nr. of polyline points.
      double precision, intent(out) :: XP, YP, ZP        !< interpolated point coordinates at distance TP.
      double precision, intent(in)  :: TP                !< Distance from polyline start at which to place point XP,YP.
      integer,          intent(out) :: ja                !< Whether distance is within polyline length (1) or not (0).

      integer :: i
      double precision :: DT, TI
      I  = 0
   10 CONTINUE
      I  = I + 1
      JA = 0
      IF (T(I) .LE. TP) THEN
         IF (I .LE. MMAX-1) THEN
            GOTO 10
         ENDIF
      ENDIF
      JA = 1
      DT = T(I) - T(I-1)
      TI = 0D0
      IF (DT .NE. 0D0) TI = (TP  - T(I-1) ) / DT
      XP = (1D0 - TI)*X(I-1) + TI*X(I)
      YP = (1D0 - TI)*Y(I-1) + TI*Y(I)
      ZP = (1D0 - TI)*Z(I-1) + TI*Z(I)
      RETURN
      END SUBROUTINE interpolateOnPolyline
