      SUBROUTINE OSC(KEY)
      use m_devices
      use unstruc_messages
      implicit none
      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: len
      integer :: nlevel
      CHARACTER STRING*58, WRDKEY*40
      IXP = 2
      IYP = 10
      IF (NOPSYS .EQ. 1) THEN
         CALL ISCREENMODE('T',80,25,16)
      ELSE
         RETURN
      ENDIF
   10 CONTINUE
!     CALL BOTLIN(0,1,KEY)
!     CALL ITEXTCOLOURN(MNUFOR,MNUBCK)
      CALL ITEXTCOLOUR('WHITE','BLUE')
      CALL INPOPUP('ON')
      CALL InStringXY(IXP,IYP,'enter OS-command ; ',1,STRING,LEN)
      CALL INPOPUP('OFF')
      KEY = InfoInput(55)
      IF (KEY .EQ. 24) THEN
         WRDKEY = 'OS-command'
         NLEVEL = 2
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 25) THEN
         CALL HISTOR()
      ELSE IF ((KEY .EQ. 21 .OR. KEY .EQ. 22) .AND. LEN .GE. 1) THEN
         WRITE(msgbuf,'(A,A)') 'OPERATING SYSTEM COMMAND: ',STRING(:LEN)
         call msg_flush()
         CALL IOsCommand(STRING(:LEN))
      ELSE IF (KEY .EQ. 23) THEN
         IF (NOPSYS .EQ. 1) CALL ISCREENMODE('GR',NPX,NPY,NCOLR)
         KEY = 3
         RETURN
      ENDIF
      GOTO 10
      END
