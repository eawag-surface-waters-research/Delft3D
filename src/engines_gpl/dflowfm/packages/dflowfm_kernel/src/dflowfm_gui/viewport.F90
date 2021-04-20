      subroutine viewport(xs1, ys1, xs2, ys2)
      use unstruc_opengl
      implicit none
      real xs1, ys1, xs2, ys2
      IF (InOpenGLRendering) THEN
#ifdef HAVE_OPENGL
          ! screen coordinates extend
          CALL fglViewPort(int(xs1*currentWidth), int(ys1*currentHeight), int((xs2-xs1)*currentWidth), int((ys2-ys1)*currentHeight) )
#endif
      else
         call igrarea(xs1, ys1, xs2, ys2)
      endif
      end
