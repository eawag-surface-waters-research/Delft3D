      SUBROUTINE HALT3(JA)
      ! left   mouse button: 1
      ! middle mouse button: 2
      ! right  mouse button: 3
      implicit none
      integer :: ja
      integer :: numkey
!     kappen met muis
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY .GE. 251 .AND. NUMKEY .LE. 253) then
         JA = NUMKEY-251+1
!         call inflush()
      endif
      RETURN
      END
