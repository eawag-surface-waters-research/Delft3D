      SUBROUTINE DISPXP(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      double precision :: y
!     LAAT TWEEDIMENSIONALE FUNCTIE PUNTJES ZIEN
      double precision :: X(N)
      CALL SETCOL(NCOL)
      DO 10 I = 1,N
         CALL MOVABS(X(I),Y)
         CALL CIR(0d0)
   10 CONTINUE
      RETURN
      END
