      SUBROUTINE MTEXT(TEX,X,Y,NCOL)
      use unstruc_colors
      implicit none
      double precision :: heigth
            integer :: l
      integer :: ncol
      double precision :: w1
      double precision :: width
      double precision :: x
      double precision :: xt
      double precision :: y
      double precision :: yt
!     grafische text op RELATIEVE grafische posities + achtergrondje
      REAL INFOGRAPHICS, IGRCHARLENGTH
      CHARACTER TEX*(*)
      L = len_trim(TEX)
      WIDTH  = IGRCHARLENGTH(TEX(1:L))*INFOGRAPHICS(3)
      W1     = IGRCHARLENGTH(TEX(1:1))*INFOGRAPHICS(3)
      HEIGTH = INFOGRAPHICS(4)
      XT = X1 + X*(X2-X1)
      YT = Y1 + Y*(Y2-Y1)
      CALL SETCOL(KLSCL)
      CALL FBOXnop(XT-WIDTH/2,YT-HEIGTH/2,XT+WIDTH/2+w1/2,YT+HEIGTH/2)
      CALL SETCOL(NCOL)
      CALL BOXnop(XT-WIDTH/2,YT-HEIGTH/2,XT+WIDTH/2+w1/2,YT+HEIGTH/2)
      CALL DRAWTEXT(real(XT+W1/2-WIDTH/2),real(YT),TEX)
      RETURN
      END
