      SUBROUTINE HALTESC()
      implicit none
      integer :: numkey
      numkey = 0
      do while (numkey .ne. 27)
         CALL INKEYEVENTIMM(NUMKEY)
      enddo
      end
