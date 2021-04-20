      SUBROUTINE LNABSnop(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y

      IF (InOpenGLRendering) THEN
        CALL LineTo(X,Y)
      ELSE
        CALL IGRLINETO(REAL(X),REAL(y))
      ENDIF
      END
