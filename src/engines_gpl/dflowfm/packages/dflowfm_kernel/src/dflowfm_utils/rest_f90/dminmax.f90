      SUBROUTINE DMINMAX(      X, MXLAN,   XMIN,   XMAX, MAXLAN)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: maxlan
      integer :: mxlan
      double precision :: xmax
      double precision :: xmin
      double precision :: xx
!     BEPAAL MINIMUM EN MAXIMUM VAN EEN EENDIMENSIONALE ARRAY
      DOUBLE PRECISION, intent(inout)  ::  X(MAXLAN)

      IF (MXLAN .EQ. 0) THEN
         XMIN = 0
         XMAX = 0
         RETURN
      ENDIF

      XMIN =  10D20
      XMAX = -10D20
      DO 10 I = 1,MXLAN
         XX   = X(I)
         IF (XX .NE. dmiss) THEN
            XMIN = MIN(XMIN,XX)
            XMAX = MAX(XMAX,XX)
         ENDIF
   10 CONTINUE
      IF (XMIN .EQ. 10D20) XMIN = 0
      IF (XMAX .EQ.-10D20) XMAX = 0
      RETURN
      END
