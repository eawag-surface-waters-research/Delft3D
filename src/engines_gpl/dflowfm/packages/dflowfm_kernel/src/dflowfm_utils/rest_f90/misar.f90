      SUBROUTINE MISAR(H,MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax
      double precision :: H(MMAX)

      DO 10 I = 1,MMAX
         H(I) = dmiss
   10 CONTINUE
      RETURN
      END
