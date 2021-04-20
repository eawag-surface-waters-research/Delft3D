   SUBROUTINE TWEEDRIE(X,Y,XD,YD,ZD)
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   double precision :: X,Y,XD,YD,ZD
   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   IF (JVIEW .EQ. 1) THEN
      XD = X
      YD = Y
      ZD = XYZ
   ELSE IF (JVIEW .EQ. 2) THEN
      ZD = X
      YD = Y
      XD = XYZ
   ELSE IF (JVIEW .EQ. 3) THEN
      XD = X
      ZD = -Y
      YD = XYZ
   ELSE IF (JVIEW .EQ. 4) THEN
      !    CALL DVIEW(XD,YD,ZD,X,Y,Z)  ! MOET NOG INVERS MAKEN
      XD = X
      YD = Y
      ZD = XYZ
   ELSE !In all other cases (e.g. when HOWTOVIEW is not set, e.g. in the gridgeom library)
      xd = x
      yd = y
      zd = xyz
   ENDIF

   RETURN
   END SUBROUTINE TWEEDRIE
