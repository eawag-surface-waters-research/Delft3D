      SUBROUTINE DMINMX2(      X,   XMIN,   XMAX,     MC,NC,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: nmax
      double precision :: xmax
      double precision :: xmin
      double precision :: xx
!     BEPAAL MINIMUM EN MAXIMUM VAN EEN TWEEDIMENSIONALE ARRAY
      DOUBLE PRECISION :: X(MMAX,NMAX)
      IF (MC .EQ. 0 .OR. NC .EQ. 0) THEN
         XMIN = 0
         XMAX = 0
         RETURN
      ENDIF
      XMIN =  10D20
      XMAX = -10D20
      DO 10 I = 1,MC
         DO 10 J = 1,NC
            XX   = X(I,J)
            IF (XX .NE. DXYMIS) THEN
               XMIN = MIN(XX,XMIN)
               XMAX = MAX(XX,XMAX)
            ENDIF
   10 CONTINUE
      IF (XMIN .EQ. 10D20) XMIN = 0
      IF (XMAX .EQ.-10D20) XMAX = 0
      RETURN
      END
