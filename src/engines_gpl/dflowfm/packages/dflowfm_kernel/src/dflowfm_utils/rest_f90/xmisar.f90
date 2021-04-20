      SUBROUTINE XMISAR (X, MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax
      double precision :: x
      DIMENSION X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = XYMIS
   10 CONTINUE
      RETURN
      END
