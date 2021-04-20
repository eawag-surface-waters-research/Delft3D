!
      SUBROUTINE MOVABS(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y,xx,yy

      CALL DPROJECT(X,Y,XX,YY,1)
      IF (InOpenGLRendering) THEN
        CALL MoveTo(XX,YY)
      ELSE
        CALL IGRMOVETO(real(XX),real(YY))
      ENDIF
      END
