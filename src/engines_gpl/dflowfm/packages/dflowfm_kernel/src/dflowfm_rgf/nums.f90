      SUBROUTINE NUMS  (      X,     mmax,nmax, MC,     NC)
!     GEEF AANTAL SPLINES MC EN MAXIMUM AANTAL PUNTEN OP SPLINE NC
!      USE DIMENS
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mc, nc, mmax, nmax
      integer :: i, numpi
      MC = 0
      NC = 0
      DO 10 I = 1,MMAX
         CALL NUMPold  (      X,      mmax, nmax, I,  NUMPI)
         IF (NUMPI .NE. 0) THEN
            MC = I
            NC = MAX(NC,NUMPI)
         ENDIF
    10 CONTINUE
      RETURN
      END subroutine nums
