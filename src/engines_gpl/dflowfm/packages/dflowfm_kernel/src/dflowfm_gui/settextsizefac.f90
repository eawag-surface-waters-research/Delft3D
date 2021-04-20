      SUBROUTINE SETTEXTSIZEFAC(T)
      use unstruc_opengl
      implicit none
      double precision :: tsize,t
      COMMON /TEXTSIZE/ TSIZE
      IF (InOpenGLRendering) THEN
         CALL SetTextHeight(int(FontSize*T*TSIZE))
      ELSE
         CALL IGRCHARSIZE(real(T*TSIZE),real(T*TSIZE))
      ENDIF
      END
