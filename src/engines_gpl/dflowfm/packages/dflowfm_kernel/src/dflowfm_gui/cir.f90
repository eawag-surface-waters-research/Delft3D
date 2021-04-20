      SUBROUTINE CIR(R)
      use unstruc_opengl
      implicit none
      integer :: ncolnow
      double precision :: r, Hr
      COMMON /COLNOW/ NCOLNOW

      if (r == 0d0) return
      IF (InOpenGLRendering) THEN
        HR = 0.5d0*R
        CALL KREC5(dble(Xlast),dble(Ylast),HR,HR)
        !CALL SetPointSize(real(5))
        !CALL DrawPoint(xlast,ylast)
        !CALL SetPointSize(real(1))
      ELSE
         CALL IGrCircleRel(real(R))
      ENDIF
   END
