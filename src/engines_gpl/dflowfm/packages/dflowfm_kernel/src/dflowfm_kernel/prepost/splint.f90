      SUBROUTINE SPLINT(YA,Y2A,N,X,Y)
      implicit none

      integer                                               :: N     !< number of control points
      double precision, dimension(N)                        :: ya    !< control point values
      double precision, dimension(N)                        :: y2a   !< control point second order derivatives
      double precision,                         intent(in)  :: x     !< spline coordinate
      double precision,                         intent(out) :: y     !< interpolated value at prescribed spline coordinate

!     AANGEPAST VOOR GEBRUIK BIJ XA IS ENKEL 0,1,2...N-1
!     ZOEKEN KAN GESLOOPT DOOR DEFINITIE VAN XA IS 0,1,

      double precision                                      :: EPS, A,B, SPLFAC = 1D0

      integer                                               :: intx
      integer                                               :: KLO, KHI

      EPS  = 0.00001D0
      INTX = INT(X)
      IF (X-INTX .LT. EPS) THEN
         Y = YA(INTX+1)
      ELSE
         KLO = INTX + 1
         KHI = KLO  + 1
         A   = ((KHI-1)-X)
         B   = (X-(KLO-1))
         Y   = A*YA(KLO) + B*YA(KHI) + SPLFAC*( (A**3-A)*Y2A(KLO) + (B**3-B)*Y2A(KHI) )/6D0
      ENDIF
      RETURN
      END SUBROUTINE SPLINT
