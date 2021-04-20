      SUBROUTINE FBOXOLD(XB1,YB1,XB2,YB2)
      implicit none
      integer :: n
      integer :: ncolnow
      double precision :: xb1
      double precision :: xb2
      double precision :: yb1
      double precision :: yb2
      COMMON /COLNOW/ NCOLNOW
      REAL X(4), Y(4)
      N    = 4
      X(1) = real(XB1)
      X(2) = real(XB2)
      X(3) = real(XB2)
      X(4) = real(XB1)
      Y(1) = real(YB1)
      Y(2) = real(YB1)
      Y(3) = real(YB2)
      Y(4) = real(YB2)
      IF (NCOLNOW .GE. 0) CALL PFILLERCORE(X,Y,N)
      RETURN
      END
