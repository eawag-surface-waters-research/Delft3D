      SUBROUTINE DTEKTRI(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,NCOL,NCOLR)
      use gridoperations
      implicit none
      integer :: ncol
      integer :: ncolr
      double precision :: zz
      double precision :: XX(3), YY(3)
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3
      CALL DRIETWEE(X1,Y1,Z1,XX(1),YY(1),ZZ)
      CALL DRIETWEE(X2,Y2,Z2,XX(2),YY(2),ZZ)
      CALL DRIETWEE(X3,Y3,Z3,XX(3),YY(3),ZZ)
      CALL PFILLER(XX,YY,3,NCOL,NCOLR)
      RETURN
      END
