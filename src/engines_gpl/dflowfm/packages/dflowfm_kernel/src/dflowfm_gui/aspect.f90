!
      SUBROUTINE ASPECT(X1D,Y1D,X2D,Y2D)
      use m_devices
      implicit none
      double precision :: asp
      double precision :: x1d
      double precision :: x2d
      double precision :: y1d
      double precision :: y2d
!     RETURN Y2 AS 1.0 ASPECT RATIO VALUE
      CALL INQASP(ASP)
      Y2D = Y1D + (X2D - X1D)*ASP
      RETURN
      END
