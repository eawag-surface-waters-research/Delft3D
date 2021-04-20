      SUBROUTINE MISARRR(H,NUMQ,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: k
      integer :: mmax
      integer :: nmax
      integer :: numq
      double precision :: H(NUMQ,MMAX,NMAX)

      DO 10 K = 1,NUMQ
         DO 10 I = 1,MMAX
            DO 10 J = 1,NMAX
               H(K,I,J) = dmiss
   10 CONTINUE
      RETURN
      END
