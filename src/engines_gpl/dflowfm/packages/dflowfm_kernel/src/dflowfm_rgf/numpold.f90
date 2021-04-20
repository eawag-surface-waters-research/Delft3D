      SUBROUTINE NUMPold  (      X,     mmax, nmax, MP,  NUMPI)
!     GEEF AANTAL PUNTEN VAN SPLINE MP
      !USE DIMENS
      use m_missing
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mp, numpi, mmax, nmax

      integer :: j
      NUMPI = 0
      DO 10 J = 1,NMAX
         IF (X(MP,J) .NE. XYMIS) NUMPI = J
    10 CONTINUE
      RETURN
      END subroutine numpold
