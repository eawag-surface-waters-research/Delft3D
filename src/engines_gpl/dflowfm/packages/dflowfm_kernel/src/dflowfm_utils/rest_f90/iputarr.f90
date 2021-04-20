      SUBROUTINE IPUTARR(XR,X,MMAX,NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
!     DE EERSTE IN DE TWEEDE
      INTEGER XR(MMAX,NMAX), X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J) = XR(I,J)
   10 CONTINUE
      RETURN
      END
