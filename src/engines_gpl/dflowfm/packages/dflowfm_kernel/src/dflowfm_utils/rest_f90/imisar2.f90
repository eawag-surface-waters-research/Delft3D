      SUBROUTINE IMISAR2(IH,MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax
      INTEGER*2 IH(MMAX)

      DO 10 I = 1,MMAX
         IH(I) = INT(dmiss)
   10 CONTINUE
      RETURN
      END
