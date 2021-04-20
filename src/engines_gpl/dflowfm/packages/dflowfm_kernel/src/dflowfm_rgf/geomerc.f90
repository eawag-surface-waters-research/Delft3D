      SUBROUTINE GEOMERC(XG,YG,XX,YY)
      USE M_SFERIC
      implicit none
      double precision :: XX,YY,XG,YG,FI2,YC,CY,F,E
      double precision :: a
      double precision :: sf
      XX  = XG*DG2RD*RA

      FI2 = DG2RD*YG
      YY  = ( 1D0 + SIN(FI2) ) / COS(FI2)
      YY  = RA*LOG(YY)

      A    = 6378140
      XX   = XG*DG2RD*RA
      YC   = DG2RD*(90-YG)
      CY   = COS(YC)
      F    = 298.257223d0
      E    = SQRT(2/F - 1/F**2)

      YY   = -A*log( ABS(tan(YC/2)) *((1+e*CY) / (1-e*CY))**(e/2) )
      SF   = sin(YC) / SQRT( 1 - E*E*CY )

      RETURN
      END SUBROUTINE GEOMERC
