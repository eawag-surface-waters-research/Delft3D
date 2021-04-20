      SUBROUTINE MOVABSnop(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y

      IF (InOpenGLRendering) THEN
        CALL MoveTo(X,Y)
      ELSE
        CALL IGRMOVETO(real(X),real(Y))
      ENDIF
      END
