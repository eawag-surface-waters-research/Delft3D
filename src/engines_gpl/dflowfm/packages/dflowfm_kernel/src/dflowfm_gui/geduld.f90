      SUBROUTINE GEDULD()
      implicit none
      integer :: i
      integer :: numkey
      DO 10 I = 1,800
         CALL INKEYEVENTIMM(NUMKEY)
         IF (NUMKEY .NE. 0) RETURN
   10 CONTINUE
      RETURN
      END
