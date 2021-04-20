      SUBROUTINE ANCHORCLS()
      use unstruc_colors
      implicit none
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
!     ZET ANCHOR NA CLEARSCREEN
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      CALL SETXOR(1)
      CALL SETCOL(KLANK)
      CALL IGrMARKER(real(XA),real(YA),2)
      CALL SETXOR(0)

      CALL DISDIS()

      RETURN
      END
