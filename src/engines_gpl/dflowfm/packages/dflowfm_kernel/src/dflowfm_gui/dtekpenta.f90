      SUBROUTINE DTEKPENTA(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5,NCOL,NCOLR)
      use gridoperations
      implicit none
      integer :: ncol
      integer :: ncolr
      double precision :: zz
      double precision :: XX(5), YY(5)
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5
      CALL DRIETWEE(X1,Y1,Z1,XX(1),YY(1),ZZ)
      CALL DRIETWEE(X2,Y2,Z2,XX(2),YY(2),ZZ)
      CALL DRIETWEE(X3,Y3,Z3,XX(3),YY(3),ZZ)
      CALL DRIETWEE(X4,Y4,Z4,XX(4),YY(4),ZZ)
      CALL DRIETWEE(X5,Y5,Z5,XX(5),YY(5),ZZ)
      CALL PFILLER(XX,YY,5,NCOL,NCOLR)
      RETURN
      END
