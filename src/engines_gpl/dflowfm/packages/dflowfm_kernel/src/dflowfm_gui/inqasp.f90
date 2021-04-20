!
      SUBROUTINE INQASP(ASP)
      USE M_DEVICES
      implicit none
      double precision :: asp
      double precision :: dx
      double precision :: dy
      integer :: jaxis
      integer :: nunix
      double precision :: xleft
      double precision :: xright
      double precision :: ybot
      double precision :: ytop
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS
      YTOP   = MAX(0.95d0,1 - YBOT)
      XRIGHT = MAX(0.90d0,1 - XLEFT)
      DX     = XRIGHT- XLEFT
      DY     = YTOP  - YBOT
      ASP    = ( DY*dble(NPY) ) / ( DX*dble(NPX) )
      RETURN
      END
