  SUBROUTINE REMZEROELEM(LNU)
  use m_netw
  implicit none
  integer :: LNU

  integer :: k
  integer :: l

  NUML = NUML - 1        ! Administratie aanschuiven
  DO L = LNU,NUML
     KN(1,L) = KN(1,L+1)
     KN(2,L) = KN(2,L+1)
     KN(3,L) = KN(3,L+1)
  ENDDO

  DO K = 1,NUMK
     DO L = 1,NMK(K)
        IF (NOD(K)%LIN(L) .GT. LNU) NOD(K)%LIN(L) = NOD(K)%LIN(L) - 1
     ENDDO
  ENDDO

  RETURN
  END SUBROUTINE REMZEROELEM
