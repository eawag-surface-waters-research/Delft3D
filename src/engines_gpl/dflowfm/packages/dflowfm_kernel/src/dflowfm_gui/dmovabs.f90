      SUBROUTINE DMOVABS(XD,YD,ZD)
      use m_oldz
      use gridoperations
      implicit none
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL MOVABS(X,Y)
      OZ = Z
      END
