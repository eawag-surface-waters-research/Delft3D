!>    plot a statusbar in the GUI
      SUBROUTINE READYY(TEXT,AF)
      use m_devices
      use unstruc_display, only: jaGUI
      implicit none

      CHARACTER TEXT*(*), BALK*400
      double precision :: af

      integer, save :: ih
      integer, save :: ini = 0
      integer, save :: iw
      integer, save :: ixp
      integer, save :: iyp
      integer :: naf

      if ( jaGUI.ne.1 ) return

      IF (INI .EQ. 0) THEN
         INI    = 1
         IXP    = 10
         IYP    = 10
         IW     = IWS - 10 - 10
         IH     = 2
         CALL ITEXTCOLOUR('BWHITE','BLUE')
         CALL IWinAction('FCP')
         CALL IWinOpenTitle(IXP,IYP,IW,IH,TEXT)
         CALL FILLUP(BALK,' ',IW)
         CALL ITEXTCOLOUR('BLACK','BWHITE')
         CALL IWinOutStringXY(2,2,BALK(1:IW))
      ELSE
         NAF = MAX(AF*IW,1d0)
         CALL FILLUP(BALK,'X',NAF)
         CALL IWinOutStringXY(1,2,BALK(1:NAF))
      ENDIF
      IF (AF .EQ. -1) THEN
         CALL IWinClose(1)
         INI = 0
         RETURN
      ENDIF
      RETURN
      END SUBROUTINE READYY
