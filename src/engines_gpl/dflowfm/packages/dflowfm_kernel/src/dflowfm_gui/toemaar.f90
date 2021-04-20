      SUBROUTINE TOEMAAR()
      implicit none
      integer :: key
      CALL OKAY(0)
      CALL TIMLIN()
   10 CONTINUE
      CALL INFLUSH()
      CALL INKEYEVENT(KEY)
      IF (KEY .EQ. 50  .OR. (KEY .GE. 254 .AND. KEY .LE. 259)) THEN
         GOTO 10
      ELSE IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         GOTO 10
      ENDIF
      CALL TIMLIN()
      RETURN
      END
