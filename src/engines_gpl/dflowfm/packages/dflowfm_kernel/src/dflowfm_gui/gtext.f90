      SUBROUTINE GTEXT(TEX,X,Y,NCOL)
      implicit none
      integer :: ncol
      integer :: ncolnow
      double precision :: x
      double precision :: y
      COMMON /COLNOW/ NCOLNOW
!     grafische text op grafische posities
      CHARACTER TEX*(*)
      CALL SETCOL(NCOL)
      IF (NCOLNOW .GE. 0) THEN
        CALL DRAWTEXT(real(X),real(Y),TEX)
      ENDIF
      RETURN
    END
