      SUBROUTINE BOXX(X,Y,NCOL)
      implicit none
      integer :: ncol
      integer :: ncolnow
      double precision :: x
      double precision :: y
      COMMON /COLNOW/ NCOLNOW
      CALL SETCOL(NCOL)
      IF (NCOLNOW .GE. 0) CALL IGrMARKER(real(X),real(Y),3)
      RETURN
      END
