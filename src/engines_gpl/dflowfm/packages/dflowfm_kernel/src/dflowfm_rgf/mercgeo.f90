      SUBROUTINE MERCGEO(XX,YY,XG,YG)
      USE M_SFERIC
      implicit none
      double precision :: XX,YY,XG,YG,FI2
      XG  = RD2DG*XX/RA
      FI2 = ATAN(SINH(YY/RA))
      YG  = RD2DG*FI2
      RETURN
      END SUBROUTINE MERCGEO
