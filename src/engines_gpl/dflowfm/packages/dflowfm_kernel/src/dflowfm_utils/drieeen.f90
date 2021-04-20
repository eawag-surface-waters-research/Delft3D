  SUBROUTINE DRIEEEN(XD,YD,ZD,Z)
  implicit none
  integer :: jav
  integer :: jview
  double precision :: xyz
  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  DOUBLE PRECISION :: XD,YD,ZD,Z
  IF (JVIEW .EQ. 1) THEN        ! TEGEN Z-AS
     Z = ZD
  ELSE IF (JVIEW .EQ. 2) THEN   ! VAN LINKS
     Z = XD
  ELSE IF (JVIEW .EQ. 3) THEN   ! NORMAAL
     Z = YD
  ELSE IF (JVIEW .EQ. 4) THEN
     Z = XYZ
  ENDIF
  RETURN
  END SUBROUTINE DRIEEEN
