  SUBROUTINE DELLINK(LL)
  use m_netw
  implicit none
  integer :: LL

  integer :: k1
  integer :: k2
  integer :: lnu

  IF (LL .NE. 0) THEN
     K1 = KN(1,LL) ; K2 = KN(2,LL)
     CALL DELELEM(K1,K2,LNU)
     LL = 0
  ENDIF
  RETURN
  END SUBROUTINE DELLINK
