      SUBROUTINE KREC(X,Y,Z,XD)
      implicit none
      integer :: ncol
      integer :: ncolnow
      double precision :: x
      double precision :: xd
      double precision :: y
      double precision :: z
      COMMON /COLNOW/ NCOLNOW
      CALL ISOCOL(Z,NCOL)
      IF (NCOLNOW .GE. 0) call RECTANGLE(real(X-XD),real(Y-XD),real(X+XD),real(Y+XD))
      RETURN
      END
