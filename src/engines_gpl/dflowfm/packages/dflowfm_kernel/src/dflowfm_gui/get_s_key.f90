      SUBROUTINE get_s_key(JA) ! s or left mouse
      implicit none
      integer :: ja
      integer :: numkey
!     kappen met muis
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY == 115 .or. NUMKEY == 115-32 .or. NUMKEY == 251) then
         JA = 1
         call inflush()
      endif
      RETURN
      END
