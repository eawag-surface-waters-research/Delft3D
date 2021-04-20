      SUBROUTINE DISPF1(Y,DX,N,NCOL)
      implicit none
      double precision :: dx
      integer :: i
      integer :: n
      integer :: ncol
      double precision :: x
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN MET INTERVAL
      double precision :: Y(N)
      CALL SETCOL(NCOL)
      X = 0
      CALL MOVABS(X,Y(1))
      DO 10 I = 2,N
         X = X + DX
         CALL LNABS(X,Y(I))
   10 CONTINUE
      RETURN
      END
