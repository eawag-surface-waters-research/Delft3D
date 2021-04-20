      SUBROUTINE SMALLSCREEN()
      implicit none
      integer :: jaxis
      double precision :: xleft
      double precision :: xright
      double precision :: ybot
      double precision :: ytop
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS

      YTOP   = MAX(0.95d0,1 - YBOT)
      XRIGHT = MAX(0.90d0,1 - XLEFT)
      call viewport(real(XLEFT),real(YBOT),real(XRIGHT),real(YTOP))

      RETURN
      END
