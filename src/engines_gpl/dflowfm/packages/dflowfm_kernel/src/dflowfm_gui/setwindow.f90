      SUBROUTINE SETWINDOW(NSC,X1,Y1,X2,Y2,DXH,DYH)
      implicit none
      double precision :: dx
      double precision :: dxh
      double precision :: dy
      double precision :: dyh
      integer :: nsc
      integer :: numsc
      double precision :: x1
      double precision :: x1sc
      double precision :: x2
      double precision :: x2sc
      double precision :: y1
      double precision :: y1sc
      double precision :: y2
      double precision :: y2sc
      COMMON /GSCREENS/ X1SC(100),Y1SC(100),X2SC(100),Y2SC(100),NUMSC

      CALL viewport ( real(X1SC(NSC)),real(Y1SC(NSC)),real(X2SC(NSC)),real(Y2SC(NSC)) )
      DX  = (X2-X1)*0.1d0
      DY  = (Y2-Y1)*0.1d0
      DXH = DX/2d0
      DYH = DY/2d0
  !    CALL IGRUNITS( real(X1-DX),real(Y1-DY),real(X2+DX),real(Y2+DY) )
      CALL setwor( X1-DX,Y1-DY,X2+DX,Y2+DY )

      RETURN
      END
