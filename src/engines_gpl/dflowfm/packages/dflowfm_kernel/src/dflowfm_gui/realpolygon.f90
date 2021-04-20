      SUBROUTINE realPOLYGON(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: ncolnow
      real    :: X(N), Y(N)
      COMMON /COLNOW/ NCOLNOW
      CALL SETCOL(NCOL)
      call PTABS(dble(X(1)),dble(Y(1)))
      DO 10 I = 2,N
         call LNABS(dble(X(I)),dble(Y(I)))
   10 CONTINUE
      call LNABS(dble(X(1)),dble(Y(1)))
      RETURN
      END
