      SUBROUTINE MISARR(H,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      double precision :: H(MMAX,NMAX)

      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            H(I,J) = dmiss
   10 CONTINUE
      RETURN
      END
