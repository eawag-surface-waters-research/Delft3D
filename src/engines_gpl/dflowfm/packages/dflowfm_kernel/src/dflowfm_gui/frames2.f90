      SUBROUTINE FRAMES2(NCOL)
      USE M_DEVICES
      implicit none
      integer :: ncol
      CALL SETCOL(NCOL)
      CALL IGRBORDER()
      RETURN
      END
