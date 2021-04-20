      SUBROUTINE HISTOR()
      use unstruc_files
      use unstruc_display
      use unstruc_version_module, only : unstruc_company, unstruc_program
      implicit none
      integer :: ih
      integer :: infoinput
      integer :: infowindow
      integer :: ipos
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: j
      integer :: jatab
      integer :: jofnd
      integer :: k
      integer :: key
      integer :: kstart
      integer :: maxtxt
      integer :: nlevel
      integer :: numchc
      integer :: numtop
      integer :: numtxt
      integer :: numwnh

      PARAMETER (MAXTXT = 400)
      CHARACTER DIATXT(MAXTXT)*70,WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
!
      REWIND(MDIA)
      K = 0
   10 CONTINUE
      READ(MDIA,'(A)',END = 888)
      K = K + 1
      GOTO 10
  888 CONTINUE
      KSTART = K - MAXTXT + 2
      REWIND(MDIA)
!
      K = 0
      J = 1
   20 CONTINUE
      K = K + 1
      IF (K .GE. KSTART) THEN
         READ(MDIA,'(A)',END = 999) DIATXT(J)
         J = J + 1
      ELSE
         READ(MDIA,'(A)',END = 999)
      ENDIF
      GOTO 20
  999 CONTINUE
!
      BACKSPACE(MDIA)
      NUMTXT = J - 1
      JATAB  = 0
      JOFND  = 0
      NUMTOP = NUMTXT
      NUMCHC = NUMTXT
      NLEVEL = 1

!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' HISTORY')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY (1,1,'move = ,Pgup, Pgdwn, home; quit = Esc')
!
!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)
!
      NUMWNH = InfoWindow(1)
      CALL IWinSelect(NUMWNH)
!  30 CONTINUE
!     CALL SCRLPG(DIATXT,NUMTXT,NUMTOP,NUMCHC,IH)
!     CALL SCROLH(NUMCHC,DIATXT,NUMTXT,NLEVEL,IH,JOFND,JATAB)
   50 CONTINUE
      IPOS = MAX(1,NUMTXT - 10)
      CALL IWINBROWSETEXT(DIATXT,NUMTXT,10,IPOS,' ')
      KEY = INFOINPUT(55)
      IF (KEY .EQ. 24) THEN
         CALL HELP(WRDKEY,NLEVEL)
         GOTO 50
      ENDIF
!     IF (NUMCHC .NE. 0) GOTO 30
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL IWinClose(1)
      RETURN
      END
