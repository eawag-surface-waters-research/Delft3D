!
      SUBROUTINE TOWOR(NX,NY,X,Y)
      implicit none
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
      real :: rx, ry
!     GIVE WORLD COORDINATES OF SCREENCOORDINATES
      CALL IGRUNITSFROMPIXELS(NX,NY,rx, ry)
      X = dble(rx)
      Y = dble(ry)
      RETURN
      END
