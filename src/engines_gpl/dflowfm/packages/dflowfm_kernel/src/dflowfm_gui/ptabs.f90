      SUBROUTINE PTABS(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y,xx,yy
      CALL DPROJECT(X,Y,XX,YY,1)
      if (InOpenGLRendering) THEN
          CALL DrawPoint(real(Xx),real(Yy))
      ELSE
          CALL IGRPOINT(real(XX),real(YY))
      ENDIF
      END
