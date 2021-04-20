      SUBROUTINE DRCIRC(XD,YD,ZD)
      use gridoperations
      implicit none
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL RCIRC(X,Y)
      RETURN
      END
