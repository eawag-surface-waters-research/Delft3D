      SUBROUTINE TMISARR(H,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax

      double precision :: H(NMAX,-1:MMAX+1)
      DO 10 I = -1,MMAX+1
         DO 10 J = 1,NMAX
            H(J,I) = dmiss
   10 CONTINUE
      RETURN
      END
