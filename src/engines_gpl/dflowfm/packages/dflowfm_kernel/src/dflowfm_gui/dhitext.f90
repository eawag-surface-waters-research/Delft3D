      SUBROUTINE DHITEXT(IVAL,XD,YD,ZD)
      use gridoperations
      implicit none
      integer :: ival
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      !CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL DPROJECT(Xd,Yd,X,Y,1)
      CALL HITEXT(IVAL,X,Y)
      RETURN
      END
