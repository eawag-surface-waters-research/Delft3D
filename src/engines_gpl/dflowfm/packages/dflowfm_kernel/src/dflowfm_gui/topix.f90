!
      SUBROUTINE TOPIX(X,Y,NX,NY)
      implicit none
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
!     GIVE SCREEN COORDINATES OF WORLDCOORDINATES
      CALL IGRUNITSTOPIXELS(real(X),real(Y),NX,NY)
      RETURN
      END
