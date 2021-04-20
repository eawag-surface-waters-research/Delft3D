      SUBROUTINE WEAREL()
      use m_wearelt
      implicit none
      integer, save :: ini = 0
      DOUBLE PRECISION :: X0,Y0,DY

      CALL MINMAXWORLD(XMIN,YMIN,XMAX,YMAX)

      !X1   = XMIN
      !Y1   = YMIN
      !X2   = XMAX
      !CALL SETWY(X1,Y1,X2,Y2)
      !IF (INI .EQ. 1) THEN
         CALL INILCA()
      !ELSE
      !   INI = 1
      !ENDIF
      RETURN
      END
