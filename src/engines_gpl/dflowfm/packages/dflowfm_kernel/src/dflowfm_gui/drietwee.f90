   SUBROUTINE DRIETWEE(XD,YD,ZD,X,Y,Z)
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   DOUBLE PRECISION XD,YD,ZD,X,Y,Z
   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   IF (JVIEW .EQ. 1) THEN        ! NORMAL
      X = XD
      Y = YD
      Z = ZD
   ELSE IF (JVIEW .EQ. 2) THEN   ! FROM LEFT
      X = ZD
      Y = YD
      Z = XD
   ELSE IF (JVIEW .EQ. 3) THEN   ! FROM TOP
      X = XD
      Y = -ZD
      Z = YD
   ELSE IF (JVIEW .EQ. 4) THEN
      !    CALL DVIEW(XD,YD,-ZD,X,Y,Z)
      CALL DVIEW(XD,YD,-ZD,X,Y,Z)
   ELSE !In all other cases (e.g. when HOWTOVIEW is not set, e.g. in the gridgeom library)
      x = xd
      y = yd
      z = zd
   ENDIF
   RETURN
   END SUBROUTINE DRIETWEE
