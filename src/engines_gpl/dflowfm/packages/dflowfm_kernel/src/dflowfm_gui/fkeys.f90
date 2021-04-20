      SUBROUTINE FKEYS(KEY)
      implicit none
      integer :: key
      integer :: nlevel
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      IF (KEY .EQ. 24) THEN
!        F1
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 25) THEN
!        F2
         CALL HISTOR()
      ELSE IF (KEY .EQ. 26) THEN
!        F3
         CALL OSC(KEY)
      ENDIF
      RETURN
      END
