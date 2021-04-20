!
      SUBROUTINE DISP2P(X,Y,MMAX,MC,NC,NCOL)
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: ncol
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET PUNTJES
      double precision :: X(MMAX,MMAX), Y(MMAX,MMAX)
      CALL SETCOL(NCOL)
      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (X(I,J) .NE. 0) THEN
               CALL MOVABS(X(I,J),Y(I,J))
               CALL CIR(0d0)
            ENDIF
   10 CONTINUE
      RETURN
      END
