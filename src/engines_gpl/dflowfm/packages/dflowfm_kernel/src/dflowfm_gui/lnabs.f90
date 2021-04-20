      SUBROUTINE LNABS(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y,xx,yy

      CALL DPROJECT(X,Y,XX,YY,1)
      IF (InOpenGLRendering) THEN
        CALL LineTo(XX,YY)
      ELSE
        CALL IGRLINETO(REAL(XX),REAL(yy))
      ENDIF
      END
