      SUBROUTINE QNREADERROR(W1,W2,MINP)
      use unstruc_files
      implicit none
      integer :: minp
      CHARACTER W1*(*),W2*(*)

      CALL QNERROR(W1,W2,' IN FILE '//FILENAMES(MINP))
      END
