!
      SUBROUTINE DISPFP(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
!     LAAT EEN EENDIMENSIONALE FUNCTIE ZIEN MET PUNTJES
      double precision :: X(N), Y(N)
      CALL SETCOL(NCOL)
      DO 10 I = 1,N
         CALL MOVABS(X(I),Y(I))
         CALL CIR(0d0)
   10 CONTINUE
      RETURN
      END
