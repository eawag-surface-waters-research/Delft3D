      SUBROUTINE CHAROW(      X,     mmax, nmax, I1,     I2, NUMSPL)
!     VERWISSEL RIJ I1 EN I2
!      USE DIMENS
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mmax, nmax, i1, i2, numspl
      integer :: j
      double precision :: xh

      DO 10 J = 1,NUMSPL
         XH      = X(I1,J)
         X(I1,J) = X(I2,J)
         X(I2,J) = XH
    10 CONTINUE
      RETURN
      END subroutine charow
