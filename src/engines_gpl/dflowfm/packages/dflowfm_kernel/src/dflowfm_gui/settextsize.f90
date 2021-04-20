      SUBROUTINE SETTEXTSIZE()
      use unstruc_opengl
      implicit none
      double precision :: tsize
      COMMON /TEXTSIZE/ TSIZE
      IF (InOpenGLRendering) THEN
         CALL SetTextHeight(int(FontSize*TSIZE))
      ELSE
         CALL IGRCHARSIZE(real(TSIZE),real(TSIZE))
      ENDIF
      END
