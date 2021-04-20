  SUBROUTINE SETPOINT(XP,YP,ZP,K1)

  use m_netw

  implicit none
  double precision :: XP, YP, ZP
  integer :: K1
  integer :: jav
  integer :: jview
  double precision :: xyz

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  CALL TWEEDRIE(XP,YP,XK(K1),YK(K1),ZK(K1))
  IF (JVIEW .EQ. 1) THEN
     ZK(K1) = zp ! XYZ
  ELSE IF (JVIEW .EQ. 2) THEN
     XK(K1) = XYZ
  ELSE IF (JVIEW .EQ. 3) THEN
     YK(K1) = XYZ
  ENDIF
  IF (KC(K1) .EQ. 0) KC(K1) = 1
  RETURN
  END SUBROUTINE SETPOINT
