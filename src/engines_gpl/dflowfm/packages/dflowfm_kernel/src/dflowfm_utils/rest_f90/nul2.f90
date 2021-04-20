!     PLUS QNRGF
      SUBROUTINE NUL2(N1,NSMAX)
      implicit none
      integer :: i
      integer :: nsmax
      INTEGER*2 N1(NSMAX)
      DO 30 I = 1,NSMAX
         N1(I) = 0
   30 CONTINUE
      RETURN
      END
