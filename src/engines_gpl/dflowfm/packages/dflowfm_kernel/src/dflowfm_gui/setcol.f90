      SUBROUTINE SETCOL(NCOL)
      use unstruc_opengl
      implicit none
      integer :: ncol
      integer :: ncolnow
      COMMON /COLNOW/ NCOLNOW
      IF (NCOL .NE. NCOLNOW ) THEN
          CALL IGRCOLOURN(NCOL)
          CALL SetColorFromColorNr(NCOL)
      ENDIF
      NCOLNOW = NCOL
      RETURN
      END
