      SUBROUTINE QNEOFERROR(MINP)
      USE unstruc_files
      implicit none
      integer :: minp
      CALL QNERROR('UNEXPECTED END OF FILE IN ',FILENAMES(MINP),' ')
      END
