      SUBROUTINE LTEXT(TEX,NX,NY,NCOL)
      use unstruc_colors
      implicit none
      integer :: ncol
      integer :: ndraw
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
!     grafische tekst op normale text posities
      CHARACTER TEX*(*)
      COMMON /DRAWTHIS/ ndraw(50)
      X = X1 + (X2-X1)*dble(NX)/dble(IWS)
      Y = Y2 + (Y1-Y2)*dble(NY)/dble(IHS)
      IF (NDRAW(10) .EQ. 1) THEN
         CALL SETCOL(1)
      ELSE
         CALL SETCOL(KLTEX)
      ENDIF
      CALL DRAWTEXT(real(X),real(Y),TEX)
      RETURN
      END
