   SUBROUTINE GETQUAD(LN,K1,K2,K3N,K4N)
   use m_netw
   use gridoperations
   implicit none
   integer :: LN,K1,K2,K3N,K4N

   integer :: k
   integer :: k1a
   integer :: k3
   integer :: k4
   integer :: kk
   integer :: kkk
   integer :: l
   integer :: ll
   integer :: lll

   K3N = 0 ; K4N = 0

   DO K = 1,NMK(K2)
      L = NOD(K2)%LIN(K)
      IF (L == LN) CYCLE
      CALL OTHERNODE(K2,L,K3)
      DO KK = 1,NMK(K3)
         LL = NOD(K3)%LIN(KK)
         IF (LL == L) CYCLE
         CALL OTHERNODE(K3,LL,K4)
         DO KKK = 1,NMK(K4)
            LLL = NOD(K4)%LIN(KKK)
            IF (LLL == LL) CYCLE
            CALL OTHERNODE(K4,LLL,K1A)
            IF (K1A == K1) THEN
                K3N = K3 ; K4N = K4
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   END SUBROUTINE GETQUAD
