      SUBROUTINE ORTPRO2(X1,Y1,X2,Y2,X3,Y3,X4,Y4,TV,JA)
      implicit none
      double precision :: X1, Y1, X2, Y2, X3, Y3, X4, Y4, TV
      integer :: JA

      double precision :: DX, DY, R2

      JA = -1
      DX = X2 - X1
      DY = Y2 - Y1
      R2 = (DX*DX + DY*DY)
      TV = (X3*DX + Y3*DY - X1*DX - Y1*DY) / R2
      X4 = X1 + TV*DX
      Y4 = Y1 + TV*DY
      IF (0D0 .LE. TV .AND. TV .LE. 1D0) JA = 1
      TV = TV * SQRT(R2)
      RETURN
      END SUBROUTINE ORTPRO2
