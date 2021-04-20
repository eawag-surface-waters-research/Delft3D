!
      SUBROUTINE FRAMES(NCOL)
      USE M_DEVICES
      implicit none
      integer :: ncol
      IF (NOPSYS .Ge. 2) RETURN
      CALL SETCOL(NCOL)
      CALL IGRBORDER()
      RETURN
      END
