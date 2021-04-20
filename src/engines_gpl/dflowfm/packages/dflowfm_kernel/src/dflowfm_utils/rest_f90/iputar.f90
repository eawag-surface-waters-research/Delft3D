      SUBROUTINE IPUTAR  (IXR,IX,MMAX)
      implicit none
      integer :: i
      integer :: ix
      integer :: ixr
      integer :: mmax
!     DE EERSTE IN DE TWEEDE
      DIMENSION IXR(MMAX) , IX(MMAX)
      DO 10 I = 1,MMAX
         IX(I) = IXR(I)
   10 CONTINUE
      RETURN
      END
