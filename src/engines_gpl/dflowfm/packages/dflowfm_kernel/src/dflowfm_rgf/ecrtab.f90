      SUBROUTINE ECRTAB(X,MC,NC,MRGF,HALF,mmax,nmax)
      implicit none
      double precision :: af
      double precision :: half
      integer :: i
      integer :: i4
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: mrgf
      integer :: nc
      integer :: nmax
!     SCHRIJFROUTINE RGF-FORMAT
      double precision :: X(MMAX,NMAX)
      DO 1 J=1,NC
         AF = HALF + 0.5d0*dble(J)/dble(NC)
         CALL READYY(' ',AF)
         WRITE(MRGF,888) J,(X(I,J),I=1,MC)
  1   CONTINUE
  888 FORMAT(' ETA= ',I4,5ES26.18:/(10X,5ES26.18))
      RETURN
      END SUBROUTINE ECRTAB
