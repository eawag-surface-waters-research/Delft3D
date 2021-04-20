      SUBROUTINE ANCHOR(X,Y)
      use unstruc_colors
      use m_flow, only: nplot
      use m_GlobalParameters, only: INDTP_ALL
      implicit none
      integer :: jashow
      integer :: jmouse
      integer :: ma
      integer :: na
      integer :: k
      double precision :: x, y, xa, ya, xlc, ylc, xx, yy
      real             :: xr, yr
 !    VEEG OUDE CROSS UIT EN ZET NIEUWE
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      IF (X .EQ. 0 .AND. Y .EQ. 0) THEN
         MA = 25
         NA = 40
         CALL TOWOR(MA,NA,XA,YA)
      ELSE
         CALL SETXOR(1)
         CALL SETCOL(KLANK)
         call dPROJECT(xa,ya,xx,yy,1) ; xr = xx ; yr = yy
         CALL IGrMARKER(xr,yr,2)
         CALL SETXOR(0)
         XA = X
         YA = Y
      ENDIF

      call inflowcell(XA,YA,k,1,INDTP_ALL) ! Use anchor for new nplot point (vertical profile)
      if (k > 0) nplot = k

      CALL SETXOR(1)
      CALL SETCOL(KLANK)
      call dPROJECT(xa,ya,xx,yy,1) ; xr = xx ; yr = yy
      CALL IGrMARKER(xr,yr,2)
      CALL SETXOR(0)

      CALL DISDIS()

      RETURN
      END
