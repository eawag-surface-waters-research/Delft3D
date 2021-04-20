      SUBROUTINE WAITSECS(NSEC)
      implicit none
      integer :: i
      integer :: key
      integer :: nsec
      CALL INFLUSH()
      DO 10 I = 1,NSEC
         CALL IOSWAIT(100)
         CALL INKEYEVENTIMM(KEY)
         IF (KEY .NE. -999 .AND. KEY .NE. -32387) RETURN
   10 CONTINUE
      RETURN
      END
