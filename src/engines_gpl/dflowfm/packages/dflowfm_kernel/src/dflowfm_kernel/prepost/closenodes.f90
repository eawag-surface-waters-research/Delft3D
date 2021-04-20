  SUBROUTINE CLOSENODES(K,KK,JA) ! ARE THESE NODES CLOSE, BUT UNCONNECTED?

  use m_netw
  use m_wearelt
  use gridoperations

  implicit none
  INTEGER          :: K,KK,JA

  integer :: k2
  integer :: l1
  integer :: n
  integer :: nx

  DOUBLE PRECISION :: R0, R1, R2, DLENGTH, SHORTESTLINK
  JA = 0
  R0 = DLENGTH(K,KK)
  IF (R0 > 6d0*RCIR ) RETURN

  L1 = NOD(K)%LIN(1)
  R1 = SHORTESTLINK(K) ; R2 = SHORTESTLINK(KK); R1 = MIN(R1,R2)*0.4d0
  CALL CLOSEENOUGH(XK(K), YK(K), XK(KK), YK(KK), R1, JA)
  IF (JA == 0) RETURN

  JA = 0
  NX = SIZE(NOD(K)%LIN)
  DO N = 1, NX
     L1 = NOD(K)%LIN(N)
     CALL OTHERNODE(K,L1,K2)
     IF (K2 == KK) THEN
        JA = 0 ; RETURN
     ENDIF
  ENDDO

  NX = SIZE(NOD(KK)%LIN)
  DO N  = 1, NX
     L1 = NOD(KK)%LIN(N)
     CALL OTHERNODE(KK,L1,K2)
     IF (K2 == K) THEN
        JA = 0 ; RETURN
     ENDIF
  ENDDO
  JA = 1 ! KENNELIJK UNCONNECTED

  RETURN
  END SUBROUTINE CLOSENODES
