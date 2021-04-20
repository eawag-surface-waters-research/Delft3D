      SUBROUTINE DKCIR(XD,YD,ZD,V)
      use gridoperations
      implicit none
      double precision :: v
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL KCIR(X,Y,V)
      RETURN
      END
