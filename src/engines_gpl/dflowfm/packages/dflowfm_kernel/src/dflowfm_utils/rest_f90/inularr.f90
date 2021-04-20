      SUBROUTINE INULARR(X,   MMAX,   NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      INTEGER X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J)    = 0d0
   10 CONTINUE
      RETURN
      END
