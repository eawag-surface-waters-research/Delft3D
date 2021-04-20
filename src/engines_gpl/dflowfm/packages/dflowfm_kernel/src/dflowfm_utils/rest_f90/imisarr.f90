      SUBROUTINE IMISARR(IH,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      INTEGER IH(MMAX,NMAX)

      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            IH(I,J) = dmiss
   10 CONTINUE
      RETURN
      END
