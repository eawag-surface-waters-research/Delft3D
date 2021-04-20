  SUBROUTINE GIVENEWLINKNUM(LNU)
  use m_netw
  use gridoperations
  implicit none
  integer :: LNU

  integer :: kx
  integer :: lx

  IF ( NUML == LMAX ) THEN
     KX = 1.2*NUMK ; LX = 1.2*NUML
     CALL INCREASENETW(KX, LX)
  ENDIF
  NUML = NUML + 1
  LNU  = NUML
  RETURN
  END SUBROUTINE GIVENEWLINKNUM
