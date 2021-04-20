      SUBROUTINE FIXDDBOUNDARIES()
      use m_grid
      implicit none
      integer :: i
      integer :: m
      integer :: m1
      integer :: m2
      integer :: mb
      integer :: mb2
      integer :: md
      integer :: n
      integer :: n1
      integer :: n2
      integer :: nb
      integer :: nb2
      integer :: nd
      integer :: npt
      integer :: npt2
      integer :: nputo
      COMMON /DOMBND/ MB(80),NB(80),MB2(80),NB2(80), NPT,NPT2,NPUTO
      DO 10 I = 1,NPT-1,2
         M1 = MB(I)
         N1 = NB(I)
         M2 = MB(I+1)
         N2 = NB(I+1)
         IF (M1 .GT. M2 .OR. N1 .GT. N2) THEN
            MB(I)   = M2
            NB(I)   = N2
            MB(I+1) = M1
            NB(I+1) = N1
         ENDIF
         M1 = MB(I)
         N1 = NB(I)
         M2 = MB(I+1)
         N2 = NB(I+1)
         MD = M2-M1
         ND = N2-N1
         IF (MD .GT. 0) THEN
            N = N1
            DO 20 M = M1,M2
               IJC(M,N) = -IJC(M,N)
    20      CONTINUE
         ELSE IF (ND .GT. 0) THEN
            M = M1
            DO 30 N = N1,N2
               IJC(M,N) = -IJC(M,N)
    30      CONTINUE
         ENDIF
    10 CONTINUE
      RETURN
      END SUBROUTINE FIXDDBOUNDARIES
