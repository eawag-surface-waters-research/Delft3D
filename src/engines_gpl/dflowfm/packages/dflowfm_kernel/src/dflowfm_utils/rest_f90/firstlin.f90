      SUBROUTINE FIRSTLIN(MRGF)
      use unstruc_version_module, only: unstruc_version_full, get_unstruc_source
      implicit none
      integer :: mrgf

      CHARACTER TEX*255, RUNDAT*20
      CALL DATUM(RUNDAT)
      WRITE(MRGF,'(A)') '* '//trim(unstruc_version_full)
      call get_unstruc_source(TEX)
      WRITE(MRGF,'(A)') '* Source: '//trim(TEX)
      TEX = '* File creation date: ' //RUNDAT
      WRITE(MRGF,'(A)') trim(TEX)

      RETURN
      END
