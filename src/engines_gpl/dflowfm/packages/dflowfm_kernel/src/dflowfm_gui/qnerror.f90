 !>   write an error-message to the log-file and GUI
      SUBROUTINE QNERROR(W1,W2,W3)
      use unstruc_messages
      use m_devices
      use unstruc_model, only:MD_AUTOSTARTSTOP, md_jaAutoStart
      use unstruc_display, only: jaGUI
      implicit none

      character(len=*), intent(in) :: W1, W2, W3

      integer :: infoattribute
      integer :: key
      integer :: nbck
      integer :: nfor
      integer :: nLEVEL
      character(len=600) :: REC, rec2

      COMMON /HELPNOW/   WRDKEY,NLEVEL
      CHARACTER WRDKEY*40

      REC = trim(W1) // ' ' // trim(W2) // ' ' // trim(W3)

      if (len_trim(W2) == 0) then
         rec2 = msgbuf
      else
         rec2 = ' '
      endif
      msgbuf = REC

      ! No user dialog in batchmode runs:
      if ( jaGUI == 1 .and. md_jaAutoStart /= MD_AUTOSTARTSTOP) then
         call warn_flush()
   !     inquire current colors
         NFOR = InfoAttribute(13)
         NBCK = InfoAttribute(14)
         CALL IWinAction   ('FCP')
   !     set error color
         CALL ITEXTCOLOUR('BWHITE','RED')
         CALL IWinOpen   (1,IHS-2,IWS,3)
         CALL IWINOUTSTRINGXY(IWS-15,3,'press any key')
         CALL OKAY(0)
         CALL ITEXTCOLOUR('BLUE','BWHITE')
         CALL IWINOutCentre   (2,trim(REC))

         if (len_trim(rec2) > 0) then
            CALL IWINOutCentre   (3, trim(rec2) )
         endif

      10 CONTINUE
   !     CALL INFLUSH()
         CALL INKEYEVENT(KEY)
         IF (KEY .EQ. 50  .OR. (KEY .GE. 254 .AND. KEY .LE. 259)) THEN
            GOTO 10
         ELSE
            CALL GETKEY2(KEY)
            IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
               WRDKEY = REC
               NLEVEL = 4
               CALL FKEYS(KEY)
               GOTO 10
            ENDIF
         ENDIF

         CALL IWinClose (1)
   !                            reset colors
         CALL ITEXTCOLOURN(NFOR, NBCK)

      else

         call mess (LEVEL_ERROR, trim(msgbuf) )

      endif

      RETURN
      END
