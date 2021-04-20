      SUBROUTINE NULAR (X, MMAX)
      implicit none
      integer :: i
      integer :: mmax
      double precision :: x
      DIMENSION X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = 0d0
   10 CONTINUE
      RETURN
      END
