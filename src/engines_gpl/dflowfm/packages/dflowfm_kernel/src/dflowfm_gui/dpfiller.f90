      SUBROUTINE DPFILLER(X,Y,Z,N,NCOL,NCOLR)
      use gridoperations
      implicit none
      integer :: k
      integer :: n
      integer :: ncol
      integer :: ncolr
      double precision :: zz
      DOUBLE PRECISION X(N),Y(N),Z(N)
      double precision :: XX(100), YY(100)
      DO K = 1,N
         CALL DRIETWEE(X(K),Y(K),Z(K),XX(K),YY(K),ZZ)
      ENDDO
      CALL PFILLER(XX,YY,N,NCOL,NCOLR)
      RETURN
      END
