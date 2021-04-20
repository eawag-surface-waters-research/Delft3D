  DOUBLE PRECISION FUNCTION SHORTESTLINK(K)
  use m_netw
  implicit none
  INTEGER :: K

  integer :: k1
  integer :: k2
  integer :: l1
  double precision :: r1
  INTEGER :: KK, L, NX
  DOUBLE PRECISION :: DLENGTH

  SHORTESTLINK = 1D9
  NX = SIZE(NOD(K)%LIN)
  DO KK = 1, NX
     L1 = NOD(K)%LIN(KK)
     IF (L1 .NE. 0) THEN
        K1 = KN(1,L1) ; K2 = KN(2,L1)
        IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
           R1 = DLENGTH( K1, K2 )
           SHORTESTLINK = MIN(SHORTESTLINK, R1)
        ENDIF
     ENDIF
  ENDDO
  RETURN
  END FUNCTION SHORTESTLINK
