      SUBROUTINE PUTIJ(X,XH,MMAX,NMAX,MNMAX,I1,I2,J1,J2)
      implicit none
      integer :: i
      integer :: i1
      integer :: i2
      integer :: j
      integer :: j1
      integer :: j2
      integer :: k
      integer :: mmax
      integer :: mnmax
      integer :: nmax
!     EN ZET HEM WEER TERUG

      DOUBLE PRECISION :: X(MMAX,NMAX),XH(MNMAX)
      K = 0
      DO 10 J  = J1,J2
         DO 10 I  = I1,I2
            K      = K + 1
            X(I,J) = XH(K)
    10 CONTINUE
      RETURN
      END SUBROUTINE PUTIJ
