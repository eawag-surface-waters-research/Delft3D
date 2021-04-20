    SUBROUTINE DHTEXT(VAL,XD,YD,ZD)
      use gridoperations
      implicit none
      double precision :: val
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL HTEXT(VAL,X,Y)
      RETURN
      END
