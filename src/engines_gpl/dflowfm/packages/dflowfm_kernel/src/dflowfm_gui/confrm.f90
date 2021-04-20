      SUBROUTINE CONFRM(TEXT,JAZEKR)
      use unstruc_display
      implicit none

      CHARACTER TEXT*(*)
      integer :: jazekr

      integer :: imenutwo
      integer :: infoattribute
      integer :: infoinput
      integer :: iopt
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      if ( jaGUI.ne.1 ) then
         if ( jazekr.ne.1 ) then
            jazekr=0
         end if
         return
      end if

      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
!     IXP    = INFOCURSOR(1)
!     IYP    = INFOCURSOR(2)
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL INPOPUP('ON')
   20 CONTINUE
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL TIMLIN()
      if (jazekr.eq.1) then ! SPvdP: if jazekr.eq.1, default to yes
         IOPT = IMenuTwo('NO','YES',IXP,IYP,TEXT,1,2)
      else
         IOPT = IMenuTwo('NO','YES',IXP,IYP,TEXT,1,1)
      end if
      CALL TIMLIN()
      KEY = InfoInput(55)
      CALL INFLUSH()
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
         IF (IOPT .EQ. 2) THEN
            JAZEKR = 1
         ELSE
            JAZEKR = 0
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
            JAZEKR = 0
      ELSE
         GOTO 20
      ENDIF
      CALL INPOPUP('OFF')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)

      RETURN
      END
