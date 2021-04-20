   SUBROUTINE CIRasp(R)
   use unstruc_opengl
   implicit none
   double precision :: R

   if (R == 0d0) return
   IF (InOpenGLRendering) THEN
      CALL SetPointSize(real(R))
      CALL DrawPoint(xlast,ylast)
      CALL SetPointSize(real(1))
   ELSE
      CALL IGrCircleRel(real(R))
   ENDIF
   END
