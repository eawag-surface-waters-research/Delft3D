      SUBROUTINE SPLINXY(X,Y,X2,Y2,N)
!      USE DIMENS
    implicit none
      integer :: n
      double precision :: X(N), Y(N), X2(N), Y2(N)
      CALL SPLINE(X,N,X2)
      CALL SPLINE(Y,N,Y2)
      RETURN
      END
