      SUBROUTINE QNMESSAGE(TEX)
      use unstruc_display
      use unstruc_messages
      implicit none
      integer :: ih
      integer :: iw
      integer :: ixp
      integer :: iyp
      CHARACTER TEX*(*)

      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

      WRITE (msgbuf,'(A)') TEX
      call msg_flush()

      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWINOUTCENTRE(1,TEX)
      CALL IWINOUTCENTRE(2,'press F2 to read this message')
      CALL IOSWAIT(200)
      CALL IWinClose(1)

      RETURN
      END
