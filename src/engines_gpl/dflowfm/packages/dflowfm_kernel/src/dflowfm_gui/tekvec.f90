      SUBROUTINE TEKVEC(NSC,X,Y,U,V,X1,X2,Y1,Y2,NCOL,TITLE)
      implicit none
      double precision :: dx
      double precision :: dxh
      double precision :: dy
      double precision :: dyh
      integer, save :: ini = 0
      integer :: ncol
      integer :: nsc
      integer :: numsc
      double precision :: psi0
      double precision :: u
      double precision :: v
      double precision :: vfac
      double precision :: x
      double precision :: x1
      double precision :: x1sc
      double precision :: x2
      double precision :: x2sc
      double precision :: y
      double precision :: y1
      double precision :: y1sc
      double precision :: y2
      double precision :: y2sc

      CHARACTER TITLE*(*), TEX*8
      COMMON /GSCREENS/ X1SC(100),Y1SC(100),X2SC(100),Y2SC(100),NUMSC

      INI = INI + 1

      call viewport(real(X1SC(NSC)),real(Y1SC(NSC)),real(X2SC(NSC)),real(Y2SC(NSC) ))
      DX  = (X2-X1)*0.1d0
      DY  = (Y2-Y1)*0.1d0
      DXH = DX/2d0
      DYH = DY/2d0
 !     CALL IGRUNITS( real(X1-DX),real(Y1-DY),real(X2+DX),real(Y2+DY) )
      CALL setwor( X1-DX, Y1-DY, X2+DX, Y2+DY )

      VFAC = 10
      PSI0 = 0
      CALL SETCOL(NCOL)
      CALL ARROWS(X,Y,U,V,PSI0,VFAC)

      RETURN
      END
