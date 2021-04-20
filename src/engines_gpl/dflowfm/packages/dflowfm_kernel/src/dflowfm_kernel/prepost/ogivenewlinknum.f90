  SUBROUTINE OGIVENEWLINKNUM(LNU)

  use m_netw
  use gridoperations

  implicit none
  integer :: LNU

  integer :: kx
  integer :: l
  integer :: lx

  DO L = 1,NUML
     IF (KN(1,L) .EQ. 0 .AND. KN(2,L) .EQ. 0) THEN
        LNU = L
        RETURN
     ENDIF
  ENDDO
  IF (NUML .LT. LMAX) THEN
     NUML = NUML + 1
     LNU  = NUML
  ELSE
     KX = 1.2*NUMK ; LX = 1.2*NUML
     CALL INCREASENETW(KX, LX)
  ENDIF
  RETURN
  END SUBROUTINE OGIVENEWLINKNUM
