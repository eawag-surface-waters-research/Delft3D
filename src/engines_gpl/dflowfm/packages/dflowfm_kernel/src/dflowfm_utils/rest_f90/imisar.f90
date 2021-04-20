      SUBROUTINE IMISAR(IH,MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax

      INTEGER IH(MMAX)
      DO 10 I = 1,MMAX
         IH(I) = dmiss
   10 CONTINUE
      RETURN
      END
