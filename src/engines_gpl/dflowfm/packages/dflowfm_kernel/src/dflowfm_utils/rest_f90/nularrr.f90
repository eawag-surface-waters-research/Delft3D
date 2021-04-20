      SUBROUTINE NULARRR(X,   MMAX,   NMAX,   LMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: l
      integer :: lmax
      integer :: mmax
      integer :: nmax
      double precision :: x
      DIMENSION X(MMAX,NMAX,LMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            DO 10 L = 1,LMAX
               X(I,J,L) = 0d0
   10 CONTINUE
      RETURN
      END
