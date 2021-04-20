      SUBROUTINE CHACOL(      X,     mmax, nmax, J1,     J2, NUMSPL)
!     VERWISSEL KOLOM J1 EN J2
!      USE DIMENS
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mmax, nmax, j1, j2, numspl
      integer :: i
      double precision :: xh

      DO 10 I = 1,NUMSPL
         XH      = X(I,J1)
         X(I,J1) = X(I,J2)
         X(I,J2) = XH
    10 CONTINUE
      RETURN
      END subroutine chacol
