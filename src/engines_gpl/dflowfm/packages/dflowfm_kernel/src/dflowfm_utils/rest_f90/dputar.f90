      SUBROUTINE DPUTAR (XR,X,MMAX)
      implicit none
      integer :: i
      integer :: mmax
!     DE EERSTE IN DE TWEEDE
      DOUBLE PRECISION XR(MMAX) , X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = XR(I)
   10 CONTINUE
      RETURN
      END
