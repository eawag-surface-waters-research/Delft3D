      SUBROUTINE GEDULD2(JAKNOP)
      implicit none
      integer :: i
      integer :: jaknop
      integer :: numkey
      JAKNOP = 0
      DO 10 I = 1,160000
         CALL INKEYEVENTIMM(NUMKEY)
         IF (NUMKEY .NE. -999 .AND. NUMKEY .NE. 257) THEN
            JAKNOP = 1
            RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END
