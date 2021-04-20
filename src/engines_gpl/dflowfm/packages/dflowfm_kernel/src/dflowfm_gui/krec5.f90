      SUBROUTINE KREC5(XX,YY,XD,YD)
      implicit none
      double precision :: xd
      double precision :: xx
      double precision :: yd
      double precision :: yy
      real :: X(4), Y(4)
      X(1) = XX - XD
      Y(1) = YY - YD
      X(2) = XX + XD
      Y(2) = YY - YD
      X(3) = XX + XD
      Y(3) = YY + YD
      X(4) = XX - XD
      Y(4) = YY + YD
      CALL PFILLERCORE( X, Y,4)
      RETURN
      END
