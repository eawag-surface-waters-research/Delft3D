  SUBROUTINE GIVELINKNUM(K1,K2,L)
  use m_netw
  implicit none
  integer :: K1, K2, L

  L = 0
  DO L = NUML, 1, -1
     IF (KN(1,L) .EQ. K1 .AND. KN(2,L) .EQ. K2 .OR. &
         KN(1,L) .EQ. K2 .AND. KN(2,L) .EQ. K1 ) THEN
        RETURN
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE GIVELINKNUM
