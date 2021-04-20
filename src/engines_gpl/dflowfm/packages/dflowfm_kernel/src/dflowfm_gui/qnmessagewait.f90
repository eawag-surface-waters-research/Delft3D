      SUBROUTINE QNMESSAGEWAIT(TEX)
      use unstruc_messages
      use unstruc_display
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
      CALL IWINOUTCENTRE(2,'this message will also appear in HISTORY (F2)')
      CALL WAIT()
      CALL IWinClose(1)

      RETURN
      END
