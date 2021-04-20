  SUBROUTINE ISNODEDB(KP, XP, YP)
  use m_netw
  implicit none
  integer :: KP
  DOUBLE PRECISION :: XP, YP, eps = 1d-6

  integer :: K
  KP = 0
  DO K = NUMK, 1,-1
     IF ( abs(XP-XK(K)) <  eps .AND. abs(YP-YK(K)) < eps ) THEN
         KP = K
         RETURN
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE ISNODEDB
