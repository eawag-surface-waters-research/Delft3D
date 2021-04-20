      SUBROUTINE LINEWIDTH(iW)
      use unstruc_opengl
      implicit none
      integer :: iw
      IF (InOpenGLRendering) THEN
        CALL SetLineWidth(iw)
      ELSE
        CALL IGRLINEWIDTH(iw,iw)
      ENDIF
      END SUBROUTINE
