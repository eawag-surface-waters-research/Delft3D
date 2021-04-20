      SUBROUTINE RECTANGLE(x1,y1,x2,y2)
      use unstruc_opengl
      implicit none
      real x1,y1,x2,y2
      real x(4),y(4)

      IF (InOpenGLRendering) THEN
        x(1) = x1
        x(2) = x2
        x(3) = x2
        x(4) = x1
        y(1) = y1
        y(2) = y1
        y(3) = y2
        y(4) = y2
        CALL PFILLERCORE(x,y,4)
      ELSE
        CALL IGRRECTANGLE(x1,y1,x2,y2)
      ENDIF

      END SUBROUTINE
