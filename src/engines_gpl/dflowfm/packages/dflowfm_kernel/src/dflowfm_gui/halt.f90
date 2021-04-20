      SUBROUTINE HALT(JA)
      implicit none
      integer, intent(out) :: ja
      integer :: numkey
!     kappen met ALLES
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY .NE. -999 .AND. NUMKEY .NE. 257 .AND. NUMKEY .NE. 254) JA = 1
      RETURN
      END
