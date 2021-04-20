      SUBROUTINE START_PROGRAM()
      use M_dimens
      USE M_DEVICES
      use unstruc_files
      use unstruc_startup
      use unstruc_version_module, only : unstruc_basename
      use unstruc_display, only : jaGUI
      use unstruc_messages

      implicit none

      integer :: infofile
      integer :: infohardware
      integer :: infoopsystem
      integer :: ja
      integer :: jmouse
      integer :: jscreen
      integer :: key
      integer :: nlevel
      integer :: num
      integer :: numclargs
      integer :: nwhat
      COMMON /HELPNOW/   WRDKEY, NLEVEL
      COMMON /KERN3D/    INFOFILE,NAMEGRID,NAMEFIELDI,NAMEFIELDO,GRIDAT
      COMMON /MESSAGETOSCREEN/ JSCREEN
      CHARACTER NAMEGRID*80,NAMEFIELDI*80,NAMEFIELDO*80,GRIDAT*1
      CHARACTER WRDKEY*40
      CHARACTER(len=8192) :: cmd
      integer :: cmdlen

!
      WRDKEY  = 'PROGRAM PURPOSE'
      NLEVEL  = 1
      JSCREEN = 0
      INFOFILE = 0

      CALL INIDIA(unstruc_basename)

      CALL FIRSTLIN(MDIA)
      CALL FIRSTLIN(6)

      CALL get_command(cmd, cmdlen)
      write (msgbuf, '(a,a)') 'Command: ', cmd(1:cmdlen); call msg_flush()

      if ( jaGUI.ne.1 ) return

!     initialisatiefiles
      CALL initProgram()

      if ( jaGUI.ne.1 ) return

! SPvdP: disabled mouse-check for mouseless buildserver
!      JMOUSE = INFOHARDWARE(13)
!      IF (JMOUSE .EQ. 1) THEN
!         CALL QNERROR ('NO MOUSE FOUND',' ',' ')
!         NLEVEL = 2
!         WRDKEY = 'MOUSE INSTALLATION'
!         CALL HELP(WRDKEY,NLEVEL)
!         CALL STOPINT()
!      ENDIF

      WRITE(msgbuf,*) 'MAXIMUM NUMBER OF LINKS         : ', LMAX      ; call msg_flush()
      WRITE(msgbuf,*) 'MAXIMUM NUMBER OF NODES         : ', KMAX      ; call msg_flush()
      WRITE(msgbuf,*) 'RESOLUTION GRAPHICS SCREEN      : ', NPX,  NPY ; call msg_flush()
      WRITE(msgbuf,*) 'RESOLUTION TEXT     SCREEN      : ', IWS,  IHS ; call msg_flush()
      WRITE(msgbuf,*) 'NUMBER OF COLOURS AVAILABLE     : ', NCOLR     ; call msg_flush()

   15 CONTINUE
      NUMCLARGS = INFOOPSYSTEM(2)
      IF (NUMCLARGS .GT. 0 .OR. INFOFILE .EQ. 1) RETURN
      KEY  = 0
      JA   = 2

      CALL MENUH(JA,NUM,NWHAT)
      CALL BOTLIN(JA,0,KEY)

      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         GOTO 15
      ENDIF

      RETURN
      END SUBROUTINE START_PROGRAM
