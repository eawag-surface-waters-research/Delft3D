      SUBROUTINE PUTAR  (XR,X,MMAX)
      implicit none
      integer :: i
      integer :: mmax
      double precision :: x
      double precision :: xr
!     DE EERSTE IN DE TWEEDE
      DIMENSION XR(MMAX) , X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = XR(I)
   10 CONTINUE
      RETURN
      END
