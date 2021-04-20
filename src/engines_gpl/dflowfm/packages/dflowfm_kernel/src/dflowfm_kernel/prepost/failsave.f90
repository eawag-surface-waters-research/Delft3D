      SUBROUTINE FAILSAVE()
      implicit none
      integer :: MSAV
      CALL NEWFIL(MSAV,'asave.net')
      CALL WRINET(MSAV)
      RETURN
      END SUBROUTINE FAILSAVE
