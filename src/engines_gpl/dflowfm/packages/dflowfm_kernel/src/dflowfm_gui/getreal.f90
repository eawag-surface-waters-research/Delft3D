      ! Now a double precision (double precision ::)
      SUBROUTINE GETREAL(TEXT,VALUE)
      use m_devices
      USE M_MISSING
      implicit none
      integer :: infoattribute
      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      double precision :: val
      double precision :: value
      CHARACTER WRDKEY*40, TEXT*(*)
      COMMON /HELPNOW/   WRDKEY,NLEVEL

      VAL    = VALUE
      IXP    = IWS/2
      IYP    = IHS/2
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL INPOPUP('ON')
   20 CONTINUE
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL TIMLIN()
!      CALL INDOUBLEXYDEF(IXP,IYP,TEXT,1,VAL,6,'(F6.1)')
      CALL INDOUBLEXYDEF(IXP,IYP,TEXT,1,VAL,12,'(F12.1)')
      CALL TIMLIN()
      KEY = InfoInput(55)
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         NLEVEL = 3
         WRDKEY = TEXT
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) THEN
            CALL INPOPUP('OFF')
            CALL ITEXTCOLOURN(NFORGR,NBCKGR)
            RETURN
         ENDIF
         GOTO 20
      ELSE IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN
         VALUE = VAL
      ELSE
         VALUE = dmiss
      ENDIF
      CALL INPOPUP('OFF')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
      END
