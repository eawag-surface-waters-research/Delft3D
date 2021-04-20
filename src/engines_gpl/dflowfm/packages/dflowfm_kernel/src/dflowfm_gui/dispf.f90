!
      SUBROUTINE DISPF(Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN
      double precision :: Y(N)
      CALL SETCOL(NCOL)
      CALL MOVABS(0d0,Y(1))
      DO 10 I = 1,N
         CALL LNABS(dble(I),Y(I))
   10 CONTINUE
      RETURN
      END
