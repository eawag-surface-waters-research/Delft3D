      SUBROUTINE ALLCOLOURS()
      use m_wearelt
      implicit none
      double precision :: dx
      double precision :: dxc
      double precision :: dy
      double precision :: dyc
      integer :: i
      integer :: j
      integer :: ncol
      double precision :: x
      double precision :: xc
      double precision :: xl
      double precision :: xu
      double precision :: y
      double precision :: yc
      double precision :: yl
      double precision :: yu
      NCOL = 0
      XL   = X2-0.66d0*DSIX-RCIR*4
      XU   = XL+0.66d0*DSIX
      YL   = Y1+DSIX
      YU   = Y2-DSIX
      DX   = XU-XL
      DY   = YU-YL
      DXC  = DX/20
      DYC  = DY/20
      DO 10 J = 1,16
         DO 10 I = 1,16
            X  = dble(I-1)/15d0
            Y  = dble(J-1)/15d0
            XC = XL + X*DX
            YC = YL + Y*DY
            CALL SETCOL(NCOL)
            NCOL = NCOL + 1
            CALL FBOXnop(XC-DXC,YC-DYC,XC+DXC,YC+DYC)
            CALL SETCOL(0)
            CALL  BOXnop(XC-DXC,YC-DYC,XC+DXC,YC+DYC)
   10 CONTINUE
      RETURN
      END
