      SUBROUTINE SETWOR(XW1,YW1,XW2,YW2)
      use unstruc_opengl
      implicit none
      double precision :: XW1,YW1,XW2,YW2
      IF (XW1 .EQ. XW2 .OR. YW1 .EQ. YW2) THEN
         XW2 = XW1+1
         YW2 = YW1+1
      ENDIF

      IF (InOpenGLRendering) THEN
#ifdef HAVE_OPENGL
        !  CALL fglDisable(GL_DEPTH_TEST) ! no depth
          CALL fglMatrixMode (GL_PROJECTION)
          CALL fglLoadIdentity()
          CALL fglOrtho(XW1,XW2,YW1,YW2,0,1)
          CALL fglMatrixMode (GL_MODELVIEW)
#endif
      else
          CALL IGrUnits(real(XW1),real(YW1),real(XW2),real(YW2))
      endif
      RETURN
      END
