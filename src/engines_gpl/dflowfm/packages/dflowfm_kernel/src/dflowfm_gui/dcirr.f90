      SUBROUTINE DCIRR(XD,YD,ZD,NCOL)
      use gridoperations
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL CIRR(X,Y,NCOL)
      RETURN
    END
