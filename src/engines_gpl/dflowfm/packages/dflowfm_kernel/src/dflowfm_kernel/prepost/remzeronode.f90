  SUBROUTINE REMZERONODE(KP)
  use m_netw
  implicit none
  integer :: KP

  integer :: k
  integer :: l

  NUMK = NUMK -1         ! Administratie aanschuiven
  DO K = KP,NUMK
     XK(K)  = XK(K+1)
     YK(K)  = YK(K+1)
     ZK(K)  = ZK(K+1)
!     IF (NETFLOW .EQ. 1)
     KC(K)  = KC(K+1)
     NMK(K) = NMK(K+1)
     DO L = 1,NMK(K)
        NOD(K)%LIN(L) = NOD(K+1)%LIN(L)
     ENDDO
  ENDDO

  DO L = 1,NUML
     IF (KN(1,L) .GT. KP) KN(1,L) = KN(1,L) - 1
     IF (KN(2,L) .GT. KP) KN(2,L) = KN(2,L) - 1
  ENDDO

  RETURN
  END SUBROUTINE REMZERONODE
