      SUBROUTINE MINMAXWORLD(XMI,YMI,XMA,YMA)
      ! ASPECT RATIO VAN HET DEFAULTGEBIED GOED ZETTEN
      USE M_WEARELT
      use m_sferic
      DOUBLE PRECISION :: XMI,YMI,XMA,YMA,ASPECT,XC,YC,DY,dx
      XMIN = XMI
      YMIN = YMI
      XMAX = XMA
      YMAX = YMA
      DX   =  XMAX - XMIN
      DY   =  YMAX - YMIN
      XC   =  XMIN + DX/2
      YC   =  YMIN + DY/2
      DX   = 1.2*DX
      DY   = 1.2*DY
      CALL INQASP(ASPECT)
      IF (DY .LT. ASPECT*DX) THEN
          DY  = ASPECT*DX
      ENDIF

      CALL SETWYNEW(XC,YC,DY)
      RETURN
      END
