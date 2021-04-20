      SUBROUTINE MENUV2(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
      use unstruc_files
      use m_devices
      implicit none
      integer :: imenuvertic
      integer :: infoinput
      integer :: infocursor
      integer :: ja, IXP, IYP
      integer :: key
      integer :: maxexp
      integer :: maxop
      integer :: maxopt
      integer :: nlevel
      integer :: nstart
      integer :: nwhat
      PARAMETER (MAXOP = 64)
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP),WRDKEY
      COMMON /HELPNOW/ WRDKEY,NLEVEL
!     Keuzemenu verticaal
!
      NSTART = NWHAT
   10 CONTINUE
      CALL BOTLIN(0,1,KEY)
!
      IXP = INFOCURSOR(1)
      IXP = INFOINPUT(62) - 1
      IYP = 2
      CALL TIMLIN()
      IF (NOPSYS .EQ. 1) THEN
         CALL ITEXTCOLOUR('BBLUE','BWHITE')
      ELSE
         CALL ITEXTCOLOUR('BLACK','BWHITE')
      ENDIF
      CALL INHIGHLIGHT('BWHITE','RED')
      CALL INPOPUP('ON')
      NWHAT  = IMENUVERTIC(OPTION,MAXOPT,IXP,IYP,' ',0,0,NSTART)
      CALL INPOPUP('OFF')
      CALL TIMLIN()
!
      KEY = InfoInput(55)
      IF (KEY .NE. 23) THEN
         NLEVEL = 2
         WRDKEY = OPTION(NWHAT)
      ENDIF

      IF (KEY .EQ. 21) THEN
!        INS KEY
         WRITE(msgbuf,'(A)') WRDKEY
         call msg_flush()
         JA = 0
         RETURN
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         JA = 0
         RETURN
      ELSE IF (KEY .EQ. 23 .OR. KEY .EQ. -2) THEN
!        ESC OR OUTSIDE
         JA    = 0
         NWHAT = 0
         RETURN
      ELSE IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ENDIF
      GOTO 10
!
      END
