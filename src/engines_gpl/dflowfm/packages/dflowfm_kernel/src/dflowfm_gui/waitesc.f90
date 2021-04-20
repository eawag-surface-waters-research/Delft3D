      SUBROUTINE WAITESC()
      implicit none
      integer :: key
      CALL INFLUSH()
   10 CONTINUE
      CALL INKEYEVENTIMM(KEY)
      IF (KEY .EQ. 27) RETURN
      GOTO 10
      END
