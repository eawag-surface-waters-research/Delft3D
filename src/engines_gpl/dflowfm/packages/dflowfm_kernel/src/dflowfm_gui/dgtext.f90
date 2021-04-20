      SUBROUTINE DGTEXT(TEX,XD,YD,ZD,NCOL)
      use gridoperations
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      double precision :: z
      CHARACTER TEX*(*)
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL GTEXT(TEX,X,Y,NCOL)
      RETURN
      END
