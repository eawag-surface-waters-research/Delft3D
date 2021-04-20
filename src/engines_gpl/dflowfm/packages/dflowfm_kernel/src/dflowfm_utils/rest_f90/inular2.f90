      SUBROUTINE INULAR2(X, MMAX)
      implicit none
      integer :: i
      integer :: mmax
      INTEGER*2 X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = 0d0
   10 CONTINUE
      RETURN
      END
