  SUBROUTINE OGIVENEWNODENUM(KNU)
  use m_netw
  use gridoperations
  implicit none
  integer :: KNU

  integer :: k
  integer :: kx
  integer :: lx

  DO K = 1,NUMK
     IF (KC(K) .EQ. 0) THEN
        KNU = K
        RETURN
     ENDIF
  ENDDO
  IF (NUMK .LT. SIZE(XK)) THEN
     NUMK = NUMK + 1
     KNU  = NUMK
  ELSE
     KX = 1.2*NUMK ; LX = 1.2*NUML
     CALL INCREASENETW(KX, LX)
  ENDIF
  RETURN
  END SUBROUTINE OGIVENEWNODENUM
