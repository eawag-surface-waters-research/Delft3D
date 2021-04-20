!
      SUBROUTINE SCROLH(NUMCHC,HLPTXT,NUMTXT,NLEVEL,IH,JOFND,JATAB)
      implicit none
      integer :: ih
      integer :: jatab
      integer :: jofnd
      integer :: key
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Controls NUMCHC, the desired line, 0 means exit
!     The value of NUMCHC is checked against limits in this routine
!     JOFIND : search, JATAB : keywordwindow
      CHARACTER HLPTXT(NUMTXT)*(*)
!
      CALL TIMLIN()
      CALL InKeyEvent(KEY)
      CALL TIMLIN()
      IF (KEY .EQ. 128) THEN
         CALL NEXT(-1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 129) THEN
         CALL NEXT(1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 130) THEN
         NLEVEL = MIN(4,NLEVEL + 1)
         CALL NEXT(1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 131) THEN
         NLEVEL = MAX(1,NLEVEL - 1)
         CALL NEXT(-1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 132) THEN
         NUMCHC = MAX(1,NUMCHC - IH)
      ELSE IF (KEY .EQ. 133) THEN
         NUMCHC = MIN(NUMTXT,NUMCHC + IH)
      ELSE IF (KEY .EQ. 140) THEN
         NUMCHC = 1
      ELSE IF (KEY .EQ. 141) THEN
         NUMCHC = NUMTXT
      ELSE IF (KEY .EQ. 177) THEN
         JOFND = -1
      ELSE IF (KEY .EQ. 27) THEN
         NUMCHC = 0
      ELSE IF (KEY .EQ. 9) THEN
         JATAB = 1 - JATAB
      ENDIF
      RETURN
      END
