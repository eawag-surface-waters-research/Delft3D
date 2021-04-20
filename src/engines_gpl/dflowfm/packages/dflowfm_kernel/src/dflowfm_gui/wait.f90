      SUBROUTINE WAIT()
      implicit none
      integer :: key
      CALL INFLUSH()
   10 CONTINUE
      CALL INKEYEVENTIMM(KEY)
      IF (KEY .NE. -999 .AND. KEY .NE. -32387) RETURN
      GOTO 10
      END
