      SUBROUTINE D1ARROWS(X,Y,Z,U,V,W,PSI0,VFAC)
      use gridoperations
      implicit none
      double precision :: psi0
      double precision :: vfac
      double precision :: X,Y,Z,U,V,W
      DOUBLE PRECISION XD,YD,ZD,XP,YP,ZP, &
                       UD,VD,WD,UR,VR,WR
      XD = X
      YD = Y
      ZD = Z
      UD = U
      VD = V
      WD = W
      CALL DRIETWEE(XD,YD,ZD,XP,YP,ZP)
      CALL DRIETWEE(UD,VD,WD,UR,VR,WR)
      CALL ARROWS(XP,YP,UR,VR,PSI0,VFAC)
      RETURN
      END SUBROUTINE D1ARROWS
