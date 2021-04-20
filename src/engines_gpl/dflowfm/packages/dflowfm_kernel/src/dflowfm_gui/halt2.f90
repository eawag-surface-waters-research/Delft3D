      SUBROUTINE HALT2(JA)
      implicit none
      integer :: ja
      integer :: numkey
!     kappen met muis
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY .GE. 251 .AND. NUMKEY .LE. 253) then
         JA = 1
!         call inflush()
      endif
      RETURN
      END
